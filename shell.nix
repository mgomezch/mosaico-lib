let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      mosaicoLib = pkgs.haskellPackages.callPackage ./. {};
    };
  };

in
  pkgs.lib.overrideDerivation haskellPackages.mosaicoLib (attrs: {
    buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
  })
