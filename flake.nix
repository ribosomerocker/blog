{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # ADD YOUR HASKELL PACKAGES IN THE CABAL FILE
  outputs = { self, nixpkgs }@inputs:
    let
      ghcVersion = "ghc924"; # CHOOSE YOUR VERSION
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskell.packages.${ghcVersion};
      myPkg = hpkgs.callCabal2nix "myPkg" ./. { };
      tools = with hpkgs; [ cabal-install haskell-language-server hlint
                            pkgs.haskellPackages.fourmolu pkgs.nixpkgs-fmt pkgs.haskellPackages.cabal-fmt 
                            pkgs.treefmt #pkgs.haskellPackages.haskell-dap pkgs.haskellPackages.ghci-dap 
                            #pkgs.haskellPackages.haskell-debug-adapter pkgs.dos2unix
                          ];
    in
      {
        packages.${system}.default = myPkg;

        devShells.${system}.default = hpkgs.shellFor {
          packages = p: [ myPkg ];
          nativeBuildInputs = tools; #++ [ pkgs.zlib ]; # ADD INPUTS HERE 
          withHoogle = true;
        };
      };
}
