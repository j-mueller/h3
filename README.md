# h3 

Extensible data visualisation in Haskell. 

"Extensible" means that the main feature of this library is a type class [`Scalable`](https://j-mueller.github.io/h3/h3-core-0.0.1/Data-H3-Scalable.html#t:Scalable) that specifies how scales (configurable functions) can be constructed. A number of instances are provided in [`Data.H3.Scales`](https://j-mueller.github.io/h3/h3-core-0.0.1/Data-H3-Scales.html), [`Data.H3.Visuals`](https://j-mueller.github.io/h3/h3-core-0.0.1/Data-H3-Visuals.html), [`Data.H3.Geo.Projection`](https://j-mueller.github.io/h3/h3-geo-0.0.1/Data-H3-Geo-Projection.html) and [`Data.H3.Colour`](https://j-mueller.github.io/h3/h3-colour-0.0.1/Data-H3-Colour.html). Scales can be combined using the combinators in [`Data.H3.Scales`](https://j-mueller.github.io/h3/h3-core-0.0.1/Data-H3-Scales.html) and new instances are easy to implement. The [`h3-examples`](https://j-mueller.github.io/h3/h3-examples-0.0.1/) project contains some examples. 

![docs/nest-bar-chart.png](docs/nest-bar-chart.png)

# Examples

The examples can be built with [nix](https://nixos.org/nix/). Run `nix-build -A examples.map` or `nix-build -A examples.bar-chart`. The output will be an SVG file in the symlinked folder `result`.


# License

MIT (see LICENSE file)
