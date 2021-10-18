{
  inputs.nixpkgs.url = github:poscat0x04/nixpkgs/dev;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = exh-dev.envFunc { withHoogle = true; };
            defaultPackage = exh;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          exh = hpkgs.callCabal2nix "exh" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit exh;
            exh-dev = addBuildTools exh [
              haskell-language-server
              cabal-install
            ];
            haskellPackages = hpkgs;
          };
    };
}
