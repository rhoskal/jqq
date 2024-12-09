{
  description = "A flake for Haskell development";

  inputs = {
    nixpkgs.url = "nixpkgs/release-24.11";
  };

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forEachSupportedSystem =
        f: nixpkgs.lib.genAttrs supportedSystems (system: f { pkgs = import nixpkgs { inherit system; }; });
    in
    {
      packages = forEachSupportedSystem (
        { pkgs }:
        {
          default = {
            # Package definition
          };
        }
      );

      devShells = forEachSupportedSystem (
        { pkgs }:
        {
          default = pkgs.mkShell {
            packages =
              with pkgs;
              [
                git
                haskell.compiler.ghc98
                haskell-language-server
                haskellPackages.cabal-install
                haskellPackages.ghcid
                haskellPackages.hlint
                haskellPackages.ormolu
                nixd
                nixfmt-rfc-style
                zlib
              ]
              ++ (
                if stdenv.isLinux then
                  [
                    inotify-tools
                    libnotify
                  ]
                else if stdenv.isDarwin then
                  [
                    terminal-notifier
                    darwin.apple_sdk.frameworks.CoreFoundation
                    darwin.apple_sdk.frameworks.CoreServices
                  ]
                else
                  [ ]
              );

            shellHook = ''
              mkdir -p .nix-cabal
              export CABAL_DIR=$PWD/.nix-cabal
              export PATH=$PWD/.nix-cabal/bin:$PATH
              export LC_ALL=en_US.UTF-8
            '';
          };
        }
      );
    };
}
