{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    (haskellPackages.ghcWithPackages (p: [
      p.filepath
      p.directory
      p.cabal-install
      p.xml-conduit
    ]))
  ];
  buildInputs = with pkgs; [
    wayland
  ];
}
