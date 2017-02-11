defmodule ElixirMl.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_ml,
      version: "0.1.0",
      elixir: "~> 1.4",
      docs: [
        #logo: "path/to/logo.png",
        extras: ["README.md": [path: "getting_started", title: "Getting Started"]],
        main: "getting_started",
        ],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
    ]
  end

  def application do
    # Specify extra applications you'll use from Erlang/Elixir that
    # are not a dependency listed below in `deps`
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:ex_spirit, "~> 0.2.4"},
      {:ex_doc, "~> 0.14.5", only: [:dev]},
    ]
  end
end
