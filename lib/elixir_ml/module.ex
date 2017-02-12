defmodule ElixirML.Module do
  @moduledoc """
  """


  defmacro __using__([]) do
    quote do
      import ElixirML.Module
      Module.register_attribute(
        __MODULE__,
        :elixirml_module_expressions,
        accumulate: true,
        persist: false
      )
      @before_compile ElixirML.Module
    end
  end



  defmacro __before_compile__(env) do
    quote do
      ElixirML.Module.process_exprs(unquote(Macro.escape(env)), @elixirml_module_expressions)
    end
  end


  def process_exprs(env, exprs, mlEnv \\ %{})

  def process_exprs(env, [], mlEnv) do
    IO.inspect {:MLENV, mlEnv}
    q = Enum.reduce(mlEnv, nil, fn ({name, q}, acc) ->
      IO.inspect {:BLAH, name, q, acc}
      quote do
        unquote(acc)
        unquote(q)
      end
    end)
    quote do
      Module.eval_quoted(unquote(Macro.escape(env)), unquote(Macro.escape(q)))
    end
  end

  def process_exprs(env, [expr | rest], mlEnv) do
    IO.inspect {:PROCESS, expr}
    mlEnv = process_expr(env, expr, mlEnv)
    process_exprs(env, rest, mlEnv)
  end



  defp process_expr(env, expr, mlEnv)

  defp process_expr(env, {:let, let_meta, [{name, name_meta, _}, is_rec, bindings, exprs_ast]}, mlEnv) do
    q = {:def, let_meta, [
            {name, name_meta, bindings},
            exprs_ast,
          ]}
    Map.put(mlEnv, name, q)
  end



  # Functions



  defmacro let(let_ast)

  defmacro let(let_ast) do
    # IO.inspect {:LET, let_ast}
    ElixirML.Module.let_impl(let_ast)
  end


  def let_impl(let_ast) do
    let_impl_parseBindings(let_ast)
  end


  defp let_impl_parseBindings(binding_ast, bindings \\ [], is_rec \\ false)

  defp let_impl_parseBindings({:rec, _meta, rest_bindings_ast}, [], false) do
    let_impl_parseBindings(rest_bindings_ast, [], true)
  end

  # No arg fun
  defp let_impl_parseBindings({:=, let_meta, [{name, _, _}=name_ast, expr_ast]}, [], is_rec) when is_atom(name) do
    let_impl_topFunction(let_meta, name_ast, [], is_rec, expr_ast)
  end

  # Last arg fun
  defp let_impl_parseBindings({:=, let_meta, [{bind, _, _}=binding_ast, expr_ast]}, bindings, is_rec) when is_atom(bind) do
    [name_ast | bindings] = :lists.reverse([binding_ast | bindings])
    let_impl_topFunction(let_meta, name_ast, bindings, is_rec, expr_ast)
  end

  # TODO:  Add matching tests here
  defp let_impl_parseBindings({name, name_meta, [rest]}, bindings, is_rec) when is_atom(name) do
    let_impl_parseBindings(rest, [{name, name_meta, []} | bindings], is_rec)
  end


  defp let_impl_topFunction(let_meta, name_ast, bindings, is_rec, expression_ast) do
    IO.inspect {:TOPFUNCTION, let_meta, name_ast, bindings, is_rec, expression_ast}
    expr = Macro.escape({:let, let_meta, [name_ast, is_rec, bindings, expression_ast]})
    quote do
      @elixirml_module_expressions unquote(expr)
    end
  end

end
