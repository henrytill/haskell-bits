{
  packageOverrides = super: let self = super.pkgs; in {
    haskellPackages =
      let
        lib = self.haskell.lib;
      in
        super.haskellPackages.override {
          overrides = self: super: {
            serialise = lib.overrideCabal super.serialise (oldAttrs: { doCheck = false; });
          };
        };
  };
}
