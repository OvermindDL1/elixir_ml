defmodule ElixirML.Parser do

  defmodule Context do
    defstruct filename: "<input>", location: 0, line: 0, character: 0
    def inc_pos(%{location: location, character: character} = context) do
      %{context |
        location: location + 1,
        character: character + 1,
      }
    end
    def inc_pos(%{location: location, character: character} = context, characters) do
      %{context |
        location: location + characters,
        character: character + characters,
      }
    end
    def inc_pos(%{location: location, line: line, character: character} = context, characters, lines) do
      %{context |
        location: location + characters,
        line: line + lines,
        character: character + characters,
      }
    end

    def inc_char(context, char)
    def inc_char(%{location: location, line: line, character: character} = context, ?\n) do
      %{context |
        location: location + 1,
        line: line + 1,
        character: character + 1,
      }
    end
    def inc_char(%{location: location, character: character} = context, _char) do
      %{context |
        location: location + 1,
        character: character + 1,
      }
    end
  end

  defmodule ParseException do
    defexception message: "Unknonwn parse error", context: %Context{filename: "unknown"}
  end


  @doc ~S"""
  ```elixir

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(**)", context)
    iex> {rest, tag.tag, tag.comment}
    {"", :comment, ""}

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(*some comment *) leftovers", context)
    iex> {rest, tag.tag, tag.comment}
    {" leftovers", :comment, "some comment "}

    iex> context = %ElixirML.Parser.Context{}
    iex> {:error, error, _context} = ElixirML.Parser.comment("(* unending comment", context)
    iex> error
    "Unending comment does not close"

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag], _context} = ElixirML.Parser.comment("(* How about an (* embedded comment *) there *)", context)
    iex> {rest, tag.tag, tag.comment}
    {"", :comment, " How about an (* embedded comment *) there "}

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [], _context} = ElixirML.Parser.comment("no comment", context)
    iex> rest
    "no comment"

  ```
  """
  def comment("(*"<>rest, context), do: comment_star_(rest, Context.inc_pos(context, 2), "", context)
  def comment(rest, context), do: {:ok, rest, [], context}

  defp comment_star_("*)"<>rest, context, comment, comment_context) do
    context = Context.inc_pos(context, 2)
    {:ok, rest, [%{tag: :comment, comment: comment, context: comment_context}], Context.inc_pos(context, 2)}
  end
  defp comment_star_("(*"<>rest, context, comment, comment_context) do
    embedded_comment_context = Context.inc_pos(context, 2)
    case comment_star_(rest, embedded_comment_context, "", context) do
      {:ok, rest, [tag], new_context} -> comment_star_(rest, new_context, comment<>"(*"<>tag.comment<>"*)", comment_context)
      error -> error
    end
  end
  defp comment_star_(<<c::utf8, rest::binary>>, context, comment, comment_context), do: comment_star_(rest, Context.inc_char(context, c), <<comment::binary, c::utf8>>, comment_context)
  defp comment_star_(<<>>, _context, _comment, comment_context), do: {:error, "Unending comment does not close", comment_context}



  @doc ~S"""
  ```elixir

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag], _context} = ElixirML.Parser.whitespace("(* How about an (* embedded comment *) there *)", context)
    iex> {rest, tag.tag, tag.comment}
    {"", :comment, " How about an (* embedded comment *) there "}

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag1, tag2], _context} = ElixirML.Parser.whitespace(" (* How about an (* embedded comment *) there *) (* more comments *) ", context)
    iex> {rest, tag1.tag, tag1.comment, tag2.tag, tag2.comment}
    {"", :comment, " How about an (* embedded comment *) there ", :comment, " more comments "}

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [], _context} = ElixirML.Parser.whitespace(" \t\r\n", context)
    iex> rest
    ""

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [], _context} = ElixirML.Parser.whitespace(" \t\r\nStuff", context)
    iex> rest
    "Stuff"

    iex> context = %ElixirML.Parser.Context{}
    iex> {:error, error, _context} = ElixirML.Parser.whitespace("(* How about an unending comment", context)
    iex> error
    "Unending comment does not close"

  ```
  """
  def whitespace(<<?\s::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  def whitespace(<<?\t::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  def whitespace(<<?\r::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context))
  def whitespace(<<?\n::utf8, rest::binary>>, context), do: whitespace(rest, Context.inc_pos(context, 1, 1))
  def whitespace(rest, context) do
    case comment(rest, context) do
      {:ok, rest, [], context} -> {:ok, rest, [], context} # There was no comment
      {:ok, rest, tags, context} ->
        case whitespace(rest, context) do
          {:ok, rest, more_tags, context} -> {:ok, rest, tags ++ more_tags, context}
          error -> error
        end
      error -> error
    end
  end



  @doc ~S"""
  ```elixir

    iex> context = %ElixirML.Parser.Context{}
    iex> {:ok, rest, [tag], _context} = ElixirML.Parser.expression("(* How about an (* embedded comment *) there *)", context)

  ```
  """
  def expression("let "<>rest, context, environment), do: let_binding(rest, Context.inc_pos(context), environment)
  def expression("let\t"<>rest, context, environment), do: let_binding(rest, Context.inc_pos(context), environment)
  def expression("let\r"<>rest, context, environment), do: let_binding(rest, Context.inc_pos(context), environment)
  def expression("let\n"<>rest, context, environment), do: let_binding(rest, Context.inc_pos(context, 1, 1), environment)




end
