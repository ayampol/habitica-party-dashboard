{ compiler ? "ghc863", pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
    };
  };
  drv = overriddenPackages.callCabal2nix "potato" ./habitica-party-dashboard.cabal {};
in
  {
    hpdash = drv;
    hpdash-shell = overriddenPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs;[
        haskellPackages.cabal-install
        haskellPackages.ghcid
        haskellPackages.hindent
        elmPackages.elm
        elmPackages.elm-format
      ];
    };
  }
