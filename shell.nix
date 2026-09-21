{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: [
      p.async
      p.cabal-install
      p.xml-conduit
    ]))
  ];
}
