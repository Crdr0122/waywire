{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: [
      p.filepath
      p.directory
      p.cabal-install
      p.xml-conduit
    ]))
  ];
}
