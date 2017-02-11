defmodule ElixirML.Parser.MacroHelpers do

  # defmacro defastrule({name, _, [parser_ast]}) when is_atom(name) do
  #   "ast_" <> tagStr = to_string(name)
  #   tag = String.to_atom(tagStr)
  #   quote do
  #     defrule unquote(name)(context) do
  #       case context |> unquote(parser_ast) do
  #         %{error: nil} = return_context ->
  #           # IO.inspect {:bleh, return_context}
  #           %{return_context |
  #             result: {unquote(tag), [line: context.line, context: {context, return_context}], return_context.result}
  #           }
  #         bad_context -> bad_context
  #       end
  #     end
  #   end
  # end

end

defmodule ElixirML.Parser do
  use ExSpirit.Parser, text: true
  # import ElixirML.Parser.MacroHelpers

  def wrapToAST({oldContext, newContext}, tag) do
    %{newContext |
      result: {tag, [line: oldContext.line, column: oldContext.column, context: {oldContext, newContext}], List.wrap(newContext.result)}
    }
  end

  @doc ~S"""
  Parses an unsigned integer

  ## Examples

    iex> import ElixirML.Parser
    iex> parse_value = "0x2a"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_integer)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :integer, [42], ""}

    iex> import ElixirML.Parser
    iex> parse_value = "0b101010"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_integer)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :integer, [42], ""}

    iex> import ElixirML.Parser
    iex> parse_value = "42"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_integer)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :integer, [42], ""}

  """
  defrule ast_integer(skip() |> map_context_around(wrapToAST(:integer), no_skip(alt([
    lit("0x") |> uint(16),
    lit("0b") |> uint(2),
    uint(10),
  ]))))



  @doc ~S"""
  Parse an identifier

  ## Examples

    iex> import ElixirML.Parser
    iex> parse_value = "something"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_identifier)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :identifier, ["something"], ""}

  """
  defrule ast_identifier(skip() |> map_context_around(wrapToAST(:identifier), no_skip(
    chars1([?a..?z, ?_], [?A..?Z, ?a..?z, ?0..?9, ?_])
  )))



  @doc ~S"""
  Parse an atom

  ## Examples

    iex> import ElixirML.Parser
    iex> parse_value = ":something"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_atom)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :atom, ["something"], ""}

    iex> import ElixirML.Parser
    iex> parse_value = "Something"
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_atom)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :atom, ["Something"], ""}

    iex> import ElixirML.Parser
    iex> parse_value = ":\"arbitrary atom text\""
    iex> %{error: error, result: result, rest: rest} = parse(parse_value, ast_atom)
    iex> {tag, _meta, value} = result
    iex> {error, tag, value, rest}
    {nil, :atom, ["arbitrary atom text"], ""}

  """
  defrule ast_atom(skip() |> map_context_around(wrapToAST(:atom), no_skip(
    alt([
      lit(?:) |> chars([?A..?Z, ?a..?z, ?0..?9, ?_]),
      seq([lit(":\""), chars(-?"), lit(?")]),
      chars1(?A..?Z, [?A..?Z, ?a..?z, ?0..?9, ?_]),
    ])
  )))


end
