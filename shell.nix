{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    bun
    typescript
    cabal-install
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [shake org-mode servant-server]))
  ];
}
