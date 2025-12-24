{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    bun
    typescript
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [org-mode servant-server]))
  ];
}
