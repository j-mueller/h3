let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

  dontHaddock = pkgs.haskell.lib.dontHaddock;

  ghc-imports = self: super: {

    concur-core = self.callPackage ./nix/concur-core.nix {};
    concur-react = self.callPackage ./nix/concur-react.nix {};
    format-numbers = pkgs.haskell.lib.dontCheck (super.format-numbers);
    ghcjs-base-stub = self.callPackage ./nix/ghcjs-base-stub.nix {};
    palette = self.callPackage ./nix/palette.nix {};
    readshp = self.callPackage ./nix/readshp.nix {};

    h3-core = self.callPackage ./src/h3-core/default.nix {  };
    h3-colour = self.callPackage ./src/h3-colour/default.nix {  };
    h3-concur-react = self.callPackage ./src/h3-concur-react/default.nix { };
    h3-geo = self.callPackage ./src/h3-geo/default.nix { };
    h3-svg = self.callPackage ./src/h3-svg/default.nix { };

  };

in
  rec {

    inherit pkgs;
        
    ghc822 = pkgs.haskell.packages.ghc822.override {
      overrides = ghc-imports;
    };
    ghc843 = pkgs.haskell.packages.ghc843.override {
      overrides = ghc-imports;
    };

    examples = {
      
      bar-chart = pkgs.stdenv.mkDerivation {
        name = "h3-bar-chart-example";
        src = ./examples/bar-chart;        
        buildInputs = [(ghc843.ghcWithPackages (pkgs: [ghc843.h3-core ghc843.h3-svg ghc843.lens ghc843.svg-builder ghc843.h3-colour]))  ghc843.runghc ];
        installPhase = ''runghc Main.hs; mkdir -p $out; cp out.svg $out/out.svg'';
      };

      map = pkgs.stdenv.mkDerivation {
        name = "h3-map-example";
        src = ./examples/map;        
        buildInputs = [(ghc843.ghcWithPackages (pkgs: [ghc843.h3-core ghc843.h3-svg ghc843.lens ghc843.svg-builder ghc843.h3-colour ghc843.h3-geo ghc843.filepath]))  ghc843.runghc ];
        installPhase = ''runghc Main.hs; mkdir -p $out; cp out.svg $out/out.svg'';
      };

      plot = pkgs.stdenv.mkDerivation rec {
        name = "h3-plot-example";
        src = ./examples/plot;     
        packages = [
          ghc843.h3-core 
          ghc843.h3-svg 
          ghc843.lens 
          ghc843.statistics
          ghc843.svg-builder 
          ghc843.h3-colour];
        buildInputs = [(ghc843.ghcWithPackages (pkgs: packages)) ghc843.runghc];
        installPhase = ''runghc Main.hs; mkdir -p $out; cp out.svg $out/out.svg'';
      };
      
      osm = pkgs.stdenv.mkDerivation {
        name = "h3-osm-example";
        src = pkgs.stdenv.fetchurl {
          curlOpts = "-v";
        }
      }
    };

  }