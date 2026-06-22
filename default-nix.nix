{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/emberdart/haskell-tools/archive/master.tar.gz") {
    inherit nixpkgs;
    inherit compiler;
  },
  compiler ? "ghc914"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  inherit (nixpkgs.pkgs.haskell) lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # Don't check necessarily until we can include non-Haskell stuff for tests
      archery = lib.doBenchmark (lib.dontHaddock (self.callCabal2nix "archery" (gitignore ./.) {}));
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.archery
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find . -type f | grep -v "dist-*"); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_23 nixpkgs.php85 nixpkgs.tinycc ];
    nativeBuildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_23 nixpkgs.php85 nixpkgs.tinycc ];
    withHoogle = false;
  };
  in
{
  inherit shell;
  archery = lib.justStaticExecutables myHaskellPackages.archery;
}
