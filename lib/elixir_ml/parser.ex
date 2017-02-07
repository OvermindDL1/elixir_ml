defmodule ElixirML.Parser do
  @moduledoc """
  Gragh!  Ignore the below, going to find or make a decent parser...

  This is used instead of yecc due to lack of column information in yecc as well
  as a few other minor things like not being able to change precedence of
  operators based on the parsing state and so forth...
  Erlang/Elixir needs a better lexing/parsing system...  Maybe a port of Spirit?
  """

  defmodule Context do
    defstruct filename: "<unknown>", line: 0, column: 0, characters: 0

    def add_columns(%{columns: columns} = context, chars) when is_integer(chars), do: %{context | columns: columns + chars}
    def add_columns(%{columns: columns} = context, charlist) when is_list(charlist), do: %{context | columns: columns + :erlang.length(charlist)}
  end


  def context_addchars(%Context{column: column, characters: characters} = context, chars) do
    %{context|
      column: column + chars,
      characters: characters + chars
    }
  end


  def context_addlinechars(%Context{line: line, characters: characters} = context, chars, lines) do
    %{context|
      line: line + lines,
      column: 0,
      characters: characters + chars
    }
  end

  defmodule Environment do
    defstruct bindings: %{}
  end

  defmodule ParseException do
    defexception message: "Unknonwn parse error", context: %Context{}, input: "", environment: nil

    def message(exc) do
      # IO.inspect {:EXCEPTION, exc}
      "#{exc.context.filename}:#{exc.context.line}:#{exc.context.column}: #{exc.message}\n\tInput: #{String.slice(exc.input, 0, 255)}"
    end
  end


  @doc ~S"""
    ```elixir

      iex> e = try do ElixirML.Parser.fromString("") rescue e in ElixirML.Parser.ParseException -> e end
      iex> e.context.characters
      0

      iex> e = try do ElixirML.Parser.fromString(" ") rescue e in ElixirML.Parser.ParseException -> e end
      iex> e.context.characters
      1

      iex> e = try do ElixirML.Parser.fromString("letx = 3.14 in\n\tx\n") rescue e in ElixirML.Parser.ParseException -> e end
      iex> e.context.characters
      3

      iex> {rest, context, _environment} = ElixirML.Parser.fromString("let(*skip*)x = 42 in\n\tx\n")
      iex> {rest, context.line}
      {"", 3}

      iex> {rest, context, _environment} = ElixirML.Parser.fromString("let x = 3.14 in\n\tx\n")
      iex> {rest, context.line}
      {"", " ", 3}

    ```
  """
  def fromString(input, options \\ []) do
    context = options[:context] || %Context{
      filename: (options[:filename] || "<unknown>"),
      line: (options[:line] || 1),
      column: (options[:column] || 0),
    }
    environment = %Environment{}
    {parsing_input, context, comments, _skipped, environment} = parse_skipper(input, context, environment)
    {_leftover_input, _context, _environment} = ast_expr(parsing_input, context, environment, comments)
  end


  def ensure_skipper(<<_::utf8, _::binary>>, _options), do: true
  def ensure_skipper(_skipped, options) do
    raise(ParseException, options)
  end


  def parse_exact_maybe(toMatch, rest) do
    if String.starts_with?(rest, toMatch) do
      sized = 8*byte_size(toMatch)
      <<_::size(sized), rest::binary>> = rest
      rest
    else
      nil
    end
  end

  def parse_exact(toMatch, rest, options) do
    if String.starts_with?(rest, toMatch) do
      sized = 8*byte_size(toMatch)
      <<_::size(sized), rest::binary>> = rest
      rest
    else
      raise(ParseException, options)
    end
  end


  def ast_expr("let"<>rest, context, environment, comments) do
    {rest, context, comments, skipped, environment} = parse_skipper(rest, context_addchars(context, 3), environment)
    ensure_skipper(skipped, input: rest, context: context, environment: environment)
    case ast_identifier(rest, context, environment, comments) do
      {rest, {:identifier, identifier, meta, environment} = ident_ast, context, environment} ->
        {rest, context, _comments, skipped, environment} = parse_skipper(rest, context_addchars(context, String.length(identifier)), environment)
        rest = parse_exact("=", rest, input: rest, context: context, environment: environment)
        {rest, context, comments, skipped, environment} = parse_skipper(rest, context_addchars(context, String.length(identifier)), environment)
        tag = {:function, ident_ast, context, environment}
        sub_environment = %{environment | bindings: Map.put(environment.bindings, identifier, tag)}
        {rest, expr, context, environment} = ast_expr(rest, context, sub_environment, comments)
        expr = {:function, {ident_ast, expr}, context, environment}
        next_expr =
        {rest, expr, context, environment}
      nil -> :error
    end
  end
  def ast_expr(<<c::utf8, _rest::binary>> = input, context, environment, comments) when (c>=?0 and c<=?9) or c==?. do
    ast_number(input, context, environment, comments)
  end
  def ast_expr("", context, environment, comments) do
    raise(ParseException, message: "End of input when expression was expected", context: context, input: "", environment: environment)
  end
  def ast_expr(input, context, environment, comments) do
    raise(ParseException, message: "Expected an expression but there was none", context: context, input: input, environment: environment)
  end


  def ast_number(input, context, environment, comments) do
    result = Regex.run(~r/^[0-9]+(\.[0-9]+)?/, input) |> List.first
    result_size = byte_size(result) * 8
    retcontext = context_addchars(context, result_size)
    IO.inspect {:BLOOO, result_size, result}
    <<_::size(result_size), rest::binary>> = input
    IO.inspect {:BLORP, rest}
    case Integer.parse(result) do
      {num, "."<>_} ->
        {num, ""} = Float.parse(result)
        {rest, {:float, num, [context: context]}, retcontext, environment}
      {num, ""} ->
        {rest, {:integer, num, [context: context]}, retcontext, environment}
    end
  end


  def ast_identifier(input, context, environment, comments) do
    ast_identifier(input, context, environment, comments, "", context)
  end
  def ast_identifier(<<c::utf8, rest::binary>>, context, environment, comments, "", startContext) when
  (c>=?A and c<=?Z) or
  (c>=?a and c<=?z) or
  c==?_ do
    ast_identifier(rest, context_addchars(context, 1), environment, comments, <<c::utf8>>, startContext)
  end
  def ast_identifier(<<c::utf8, rest::binary>>, context, environment, comments, identifierSoFar, startContext) when
  (c>=?A and c<=?Z) or
  (c>=?a and c<=?z) or
  (c>=?0 and c<=?9) or
  c==?_ do
    ast_identifier(rest, context_addchars(context, 1), environment, comments, <<c::utf8>>, startContext)
  end
  def ast_identifier(rest, context, environment, [], "", _startContext), do: nil
  def ast_identifier(rest, context, environment, [], identifierSoFar, startContext) do
    {rest, {:identifier, identifierSoFar, [context: startContext], environment}, context, environment}
  end
  def ast_identifier(rest, context, environment, comments, identifierSoFar, startContext) do
    {rest, {:identifier, identifierSoFar, [context: startContext, comments: comments], environment}, context, environment}
  end


  def ast_comment(text, context, environment), do: {:comment, text, context, environment}


  @doc ~S"""
    ```elixir

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper(" ", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], " ", 1}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\t", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\t", 1}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\n\r", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\n\r", 2}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\n", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\n", 2}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\r\n", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\r\n", 2}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\r", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\r", 2}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper("\n\n", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"", [], "\n\n", 3}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, comments, parsed, _env} = ElixirML.Parser.parse_skipper(" a", context, nil)
      iex> {rest, comments, parsed, context.line}
      {"a", [], " ", 1}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, [{:comment, text, _context, _env}], parsed, _env} = ElixirML.Parser.parse_skipper(" (**)", context, nil)
      iex> {rest, text, parsed, context.line}
      {"", "", " (**)", 1}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, [{:comment, text, _context, _env}], parsed, _env} = ElixirML.Parser.parse_skipper(" (* Comment (* and embedded *) *)", context, nil)
      iex> {rest, text, parsed, context.line}
      {"", " Comment (* and embedded *) ", " (* Comment (* and embedded *) *)", 1}

      iex> context = %ElixirML.Parser.Context{line: 1}
      iex> {rest, context, [{:comment, text, _context, _env}], parsed, _env} = ElixirML.Parser.parse_skipper(" (* Comment \n\t(* and embedded *)\n *)", context, nil)
      iex> {rest, text, parsed, context.line}
      {"", " Comment \n\t(* and embedded *)\n ", " (* Comment \n\t(* and embedded *)\n *)", 3}

    ```
  """
  def parse_skipper(input, context, comments \\ [], parsed \\ "", environment)
  def parse_skipper("\s"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addchars(context, 1), comments, parsed<>"\s", environment)
  end
  def parse_skipper("\t"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addchars(context, 1), comments, parsed<>"\t", environment)
  end
  def parse_skipper("\n\r"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addlinechars(context, 2, 1), comments, parsed<>"\n\r", environment)
  end
  def parse_skipper("\n"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addlinechars(context, 1, 1), comments, parsed<>"\n", environment)
  end
  def parse_skipper("\r\n"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addlinechars(context, 2, 1), comments, parsed<>"\r\n", environment)
  end
  def parse_skipper("\r"<>rest, context, comments, parsed, environment) do
    parse_skipper(rest, context_addlinechars(context, 1, 1), comments, parsed<>"\r", environment)
  end
  def parse_skipper("(*"<>rest, context, comments, parsed, environment) do
    context = context_addchars(context, 2)
    {rest, context, comment, parsed, environment} = parse_comment_body(rest, context, context, parsed<>"(*", environment)
    parse_skipper(rest, context, comments++[comment], parsed, environment)
  end
  def parse_skipper(input, context, comments, parsed, environment), do: {input, context, comments, parsed, environment}


  def parse_comment("(*"<>rest, context, comments \\ [], parsed \\ "", environment) do
    parse_comment_body(rest, context_addchars(context, 2), comments, parsed<>"(*", environment)
  end

  def parse_comment_body(input, context, startContext, parsed \\ "(*", commentSoFar \\ "", environment)
  def parse_comment_body("*)"<>rest, context, startContext, parsed, commentSoFar, environment) do
    {rest, context_addchars(context, 2), ast_comment(commentSoFar, startContext, environment), parsed<>"*)", environment}
  end
  def parse_comment_body("(*"<>rest, context, startContext, parsed, commentSoFar, environment) do
    context = context_addchars(context, 2)
    {rest, context, {:comment, text, _embeddedStartContext, _env}, parsed, environment} = parse_comment_body(rest, context, context, parsed<>"(*", environment)
    parse_comment_body(rest, context, startContext, parsed, commentSoFar<>"(*"<>text<>"*)", environment)
  end
  def parse_comment_body("\n\r"<>rest, context, startContext, parsed, commentSoFar, environment) do
    parse_comment_body(rest, context_addlinechars(context, 2, 1), startContext, parsed<>"\n\r", commentSoFar<>"\n\r", environment)
  end
  def parse_comment_body("\n"<>rest, context, startContext, parsed, commentSoFar, environment) do
    parse_comment_body(rest, context_addlinechars(context, 1, 1), startContext, parsed<>"\n", commentSoFar<>"\n", environment)
  end
  def parse_comment_body("\r\n"<>rest, context, startContext, parsed, commentSoFar, environment) do
    parse_comment_body(rest, context_addlinechars(context, 2, 1), startContext, parsed<>"\r\n", commentSoFar<>"\r\n", environment)
  end
  def parse_comment_body("\r"<>rest, context, startContext, parsed, commentSoFar, environment) do
    parse_comment_body(rest, context_addlinechars(context, 1, 1), startContext, parsed<>"\r", commentSoFar<>"\r", environment)
  end
  def parse_comment_body(<<c::utf8, rest::binary>>, context, startContext, parsed, commentSoFar, environment) do
    parse_comment_body(rest, context_addchars(context, 1), startContext, <<parsed::binary, c::utf8>>, <<commentSoFar::binary, c::utf8>>, environment)
  end


  # def fromString(input, options \\ [])
  # def fromString(input, options) when is_binary(input), do: fromString(:erlang.binary_to_list(input), options) # Would be nice to have the lexer support binaries...
  # def fromString(input, options) do
  #   line = options[:line] || 1
  #   case :elixirml_lexer.string(input, line) do
  #     {:ok, tokens, _endLine} -> fromTokens(IO.inspect(tokens), options)
  #     err -> err
  #   end
  # end
  #
  # def fromTokens(tokens, options \\ []) do
  #   context = %Context{
  #     filename: (options[:filename] || "<unknown>"),
  #     line: (options[:line] || 1),
  #     column: (options[:column] || 0),
  #   }
  #   try do
  #     ast = topmodule(tokens, context)
  #     |> IO.inspect()
  #   rescue
  #     e in ParseException -> e
  #   end
  # end
  #
  #
  # defp topmodule(tokens, context) do
  #   expr = expr(tokens, context)
  # end
  #
  #
  # defp expr([
  #   {:let, linenum_let},
  #   {:whitespace, linenum_whitespace1, whitespace_length1},
  #   {:identifier, linenum_identifier, identifier},
  #   {:whitespace, linenum_whitespace1, whitespace_length1},
  #   {:operator, line_num_equals, :=},
  #   {:whitespace, linenum_whitespace1, whitespace_length1},
  #   | rest], context) do
  #     context = context
  #     |> add_columns()
  #   {:function, %{context | lineNum_let}, [o]}
  # end
  # defp expr([token | _rest] = tokens, context) do
  #   raise ParseException, message: "Unknown expression: #{inspect token}", tokens: tokens, context: context
  # end


  # @non_name_token_chars [
  #   ?(,
  #   ?),
  #   ?[,
  #   ?],
  #   ?{,
  #   ?},
  #   ?\s,
  #   ?\t,
  #   ?\r,
  #   ?\n,
  #   ?-,
  #   ?*,
  #   ?%,
  # ]
  #
  # defmodule Context do
  #   defstruct filename: "<input>", location: 0, line: 0, character: 0
  #   def inc_pos(%{location: location, character: character} = context) do
  #     %{context |
  #       location: location + 1,
  #       character: character + 1,
  #     }
  #   end
  #   def inc_pos(%{location: location, character: character} = context, characters) do
  #     %{context |
  #       location: location + characters,
  #       character: character + characters,
  #     }
  #   end
  #   def inc_pos(%{location: location, line: line, character: character} = context, characters, lines) do
  #     %{context |
  #       location: location + characters,
  #       line: line + lines,
  #       character: character + characters,
  #     }
  #   end
  #
  #   def inc_char(context, char)
  #   def inc_char(%{location: location, line: line, character: character} = context, ?\n) do
  #     %{context |
  #       location: location + 1,
  #       line: line + 1,
  #       character: character + 1,
  #     }
  #   end
  #   def inc_char(%{location: location, character: character} = context, _char) do
  #     %{context |
  #       location: location + 1,
  #       character: character + 1,
  #     }
  #   end
  # end
  #
  # defmodule Environment do
  #   defstruct bindings: %{}
  # end
  #
  # defmodule ParseException do
  #   defexception message: "Unknonwn parse error", context: %Context{filename: "unknown"}
  # end
  #
  #
  # @doc ~S"""
  # ```elixir
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(**)", context)
  #   iex> {rest, tag.tag, tag.comment}
  #   {"", :comment, ""}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(*some comment *) leftovers", context)
  #   iex> {rest, tag.tag, tag.comment}
  #   {" leftovers", :comment, "some comment "}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:error, error, _context} = ElixirML.Parser.comment("(* unending comment", context)
  #   iex> error
  #   "Unending comment does not close"
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(* How about an (* embedded comment *) there *)", context)
  #   iex> {rest, tag.tag, tag.comment}
  #   {"", :comment, " How about an (* embedded comment *) there "}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [], _context} = ElixirML.Parser.comment("no comment", context)
  #   iex> rest
  #   "no comment"
  #
  # ```
  # """
  # def comment("(*"<>rest, context), do: comment_star_(rest, Context.inc_pos(context, 2), "", context)
  # def comment(rest, context), do: {:ok, rest, [], context}
  #
  # defp comment_star_("*)"<>rest, context, comment, comment_context) do
  #   context = Context.inc_pos(context, 2)
  #   {:ok, rest, [%{tag: :comment, comment: comment, context: comment_context}], Context.inc_pos(context, 2)}
  # end
  # defp comment_star_("(*"<>rest, context, comment, comment_context) do
  #   embedded_comment_context = Context.inc_pos(context, 2)
  #   case comment_star_(rest, embedded_comment_context, "", context) do
  #     {:ok, rest, [tag], new_context} -> comment_star_(rest, new_context, comment<>"(*"<>tag.comment<>"*)", comment_context)
  #     error -> error
  #   end
  # end
  # defp comment_star_(<<c::utf8, rest::binary>>, context, comment, comment_context), do: comment_star_(rest, Context.inc_char(context, c), <<comment::binary, c::utf8>>, comment_context)
  # defp comment_star_(<<>>, _context, _comment, comment_context), do: {:error, "Unending comment does not close", comment_context}
  #
  #
  #
  # @doc ~S"""
  # ```elixir
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [tag], _context} = ElixirML.Parser.whitespace("(* How about an (* embedded comment *) there *)", context)
  #   iex> {rest, tag.tag, tag.comment}
  #   {"", :comment, " How about an (* embedded comment *) there "}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [tag1, tag2], _context} = ElixirML.Parser.whitespace(" (* How about an (* embedded comment *) there *) (* more comments *) ", context)
  #   iex> {rest, tag1.tag, tag1.comment, tag2.tag, tag2.comment}
  #   {"", :comment, " How about an (* embedded comment *) there ", :comment, " more comments "}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [], _context} = ElixirML.Parser.whitespace(" \t\r\n", context)
  #   iex> rest
  #   ""
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, [], _context} = ElixirML.Parser.whitespace(" \t\r\nStuff", context)
  #   iex> rest
  #   "Stuff"
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:error, error, _context} = ElixirML.Parser.whitespace("(* How about an unending comment", context)
  #   iex> error
  #   "Unending comment does not close"
  #
  # ```
  # """
  # def whitespace(<<?\s::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  # def whitespace(<<?\t::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  # def whitespace(<<?\r::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  # def whitespace(<<?\n::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context, 1, 1))
  # def whitespace(rest, context) do
  #   case comment(rest, context) do
  #     {:ok, rest, [], context} -> {:ok, rest, [], context} # There was no comment
  #     {:ok, rest, tags, context} ->
  #       case whitespace(rest, context) do
  #         {:ok, rest, more_tags, context} -> {:ok, rest, tags ++ more_tags, context}
  #         error -> error
  #       end
  #     error -> error
  #   end
  # end
  #
  #
  #
  # @doc ~S"""
  # ```elixir
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, name_token, _context} = ElixirML.Parser.name_token("id", context)
  #   iex> {rest, name_token}
  #   {"", "id"}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:ok, rest, name_token, _context} = ElixirML.Parser.name_token("Largername_token", context)
  #   iex> {rest, name_token}
  #   {"", "Largername_token"}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:error, error, _context} = ElixirML.Parser.name_token("%Noname_token", context)
  #   iex> error
  #   "Expecting a name_token but there was none"
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> {:error, error, _context} = ElixirML.Parser.name_token("", context)
  #   iex> error
  #   "Expecting a name_token but there was none"
  #
  # ```
  # """
  # def name_token(all, context), do: name_token_(all, context, "")
  # defp name_token_(<<c::utf8, rest::binary>>, context, name_token) when not(c in @non_name_token_chars) do
  #   name_token_(rest, Context.inc_char(context, c), <<name_token::binary, c::utf8>>)
  # end
  # defp name_token_(_all, context, ""), do: {:error, "Expecting a name_token but there was none", context}
  # defp name_token_(rest, context, name_token), do: {:ok, rest, name_token, context}
  #
  #
  #
  # @doc ~S"""
  # ```elixir
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> env = %ElixirML.Parser.Environment{}
  #   iex> {:ok, rest, tag, _context, environment} = ElixirML.Parser.matchable("blah", context, env)
  #   iex> {rest, tag.tag}
  #   {"", :matchable}
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> env = %ElixirML.Parser.Environment{}
  #   iex> {:ok, rest, tag, _context, environment} = ElixirML.Parser.matchable("{tupleArg1, tupleArg2}", context, env)
  #   iex> {rest, tag.tag}
  #   {"", :matchable}
  #
  # ```
  # """
  # def matchable(rest, context, environment) do
  #   case name_token(rest, context) do
  #     {:ok, rest, name_token, context} -> {:ok, rest, %{tag: :matchable, matchable: :name_token, name_token: name_token}, context, environment}
  #     _ -> :not_yet_implemented_matchable
  #   end
  # end
  #
  #
  #
  # @doc ~S"""
  # ```elixir
  #
  #   iex> context = %ElixirML.Parser.Context{}
  #   iex> env = %ElixirML.Parser.Environment{}
  #   iex> {:ok, rest, [tag], _context, environment} = ElixirML.Parser.expression("let blah = 42 in", context, env)
  #   iex> {rest, tag.tag, environment.bindings.blah}
  #   {"", :let, 42}
  #
  # ```
  # """
  # def expression(rest, context, environment) do
  #   with\
  #     {:ok, rest, whitespace_tags, context} <- whitespace(rest, context),
  #     {:ok, rest, name_token, context} <- name_token(rest, context)
  #   do expression_(name_token, rest, context, environment, whitespace_tags)
  #   end
  # end
  #
  # defp expression_("let", rest, context, environment, pre_whitespace_tags) do
  #   with\
  #     {:ok, rest, whitespace_tags, context} <- whitespace(rest, context),
  #     {:ok, rest, matchable, context, environment} <- matchable(rest, context, environment)
  #   do :not_implemented_let
  #   end
  # end
  #
  # defp expression_(name_token, rest, context, environment, pre_whitespace_tags), do: :error


end
