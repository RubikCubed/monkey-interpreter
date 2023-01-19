{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }@inputs:
    let
      ghcVersion = "ghc924";
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ghc = pkgs.haskell.packages.${ghcVersion};
      myPkg = ghc.callCabal2nix "myPkg" ./. { };
      tools = with ghc; [ cabal-install haskell-language-server hlint ];
    in
      {
        packages.${system}.default = myPkg;

        devShells.${system}.default = ghc.shellFor {
          packages = p: [ myPkg ];
          nativeBuildInputs = tools ++ [ pkgs.readline ];
        };
      };
}
