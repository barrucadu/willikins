{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      formatter.${system} = pkgs.nixpkgs-fmt;

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          black
          google-cloud-sdk
          haskellPackages.cabal-install
          haskellPackages.ghc
          haskellPackages.hlint
          (python3.withPackages (ps: [ps.discordpy]))
        ];
        buildInputs = with pkgs; [
          zlib
        ];
      };

      packages.${system}.default = pkgs.haskellPackages.developPackage {
        root = ./.;
      };

      apps.${system}.lint = {
        type = "app";
        program = toString (pkgs.writeShellScript "lint.sh" "${pkgs.haskellPackages.hlint}/bin/hlint .");
      };
    };
}
