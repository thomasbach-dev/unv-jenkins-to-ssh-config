{
  description = "A very basic flake";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/75029d4231f1db9708e57feaa9d401eb1d831d19";
    flake-utils.url = "github:numtide/flake-utils/74f7e4319258e287b0f9cb95426c9853b282730b";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          jenkins-to-ssh-config =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hspec-discover = {};
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.jenkins-to-ssh-config.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."jenkins-to-ssh-config:exe:jenkins-to-ssh-config";
    });
}
