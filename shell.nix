{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    wayland-scanner
    (haskellPackages.ghcWithPackages (p: [
      p.cabal-install
      p.xml-conduit
      p.hxt
    ]))
  ];
  buildInputs = with pkgs; [
    wayland
  ];
}
