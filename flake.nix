{
  description = "dirforest";
  nixConfig = {
    extra-substituters = "https://dirforest.cachix.org";
    extra-trusted-public-keys = "dirforest.cachix.org-1:C/TnLXGIkOL7jrhIZo95ahDttiZIc6XPaMV9xWQJddY=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.dirforestRelease;
      checks.${system} = {
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "dirforest-shell";
        packages = p: (builtins.attrValues p.dirforestPackages);
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci.cachix = {
        name = "dirforest";
        public-key = "dirforest.cachix.org-1:C/TnLXGIkOL7jrhIZo95ahDttiZIc6XPaMV9xWQJddY=";
      };
    };
}
