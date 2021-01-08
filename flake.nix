{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
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
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
              quickjs-hs = with hself; callPackage ./quickjs-hs.nix {};
            };
          };
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
