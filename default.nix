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

  h3-core = ./src/h3-core/default.nix;
  h3-colour = ./src/h3-colour/default.nix;
  h3-concur-react = ./src/h3-concur-react/default.nix;
  h3-geo = ./src/h3-geo/default.nix;
  h3-examples = ./src/h3-examples/default.nix;
  h3-svg = ./src/h3-svg/default.nix;

in
  rec {

    inherit pkgs;
    
    ghc = pkgs.haskell.packages.ghc843.override {
      overrides = self: super: {

        concur-core = self.callPackage ./nix/concur-core.nix {};
        concur-react = self.callPackage ./nix/concur-react.nix {};
        format-numbers = pkgs.haskell.lib.dontCheck (super.format-numbers);
        ghcjs-base-stub = self.callPackage ./nix/ghcjs-base-stub.nix {};
        palette = self.callPackage ./nix/palette.nix {};
        readshp = self.callPackage ./nix/readshp.nix {};

        h3-core = self.callPackage h3-core {  };
        h3-colour = self.callPackage h3-colour {  };
        h3-concur-react = self.callPackage h3-concur-react { };
        h3-geo = self.callPackage h3-geo { };
        h3-svg = self.callPackage h3-svg { };

      };
    };

    examples = {
      
      bar-chart = pkgs.stdenv.mkDerivation {
        name = "h3-bar-chart-example";
        src = ./examples/bar-chart;        
        buildInputs = [(ghc.ghcWithPackages (pkgs: [ghc.h3-core ghc.h3-svg ghc.lens ghc.svg-builder ghc.h3-colour]))  ghc.runghc ];
        installPhase = ''runghc Main.hs; mkdir -p $out; cp out.svg $out/out.svg'';
      };

      map = pkgs.stdenv.mkDerivation {
        name = "h3-map-example";
        src = ./examples/map;        
        buildInputs = [(ghc.ghcWithPackages (pkgs: [ghc.h3-core ghc.h3-svg ghc.lens ghc.svg-builder ghc.h3-colour ghc.h3-geo ghc.filepath]))  ghc.runghc ];
        installPhase = ''runghc Main.hs; mkdir -p $out; cp out.svg $out/out.svg'';
      };
    };

  }