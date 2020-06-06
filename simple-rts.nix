{ stdenv, runCommand, pkgconfig, SDL2, elfkickers, simple-rts
}:
stdenv.mkDerivation {
  name = "simple-rts-c";
  nativeBuildInputs = [ pkgconfig ];
  src = runCommand "simple-rts-src" {envVariable = true;} ''
    ${simple-rts}/bin/simple-rts
    mkdir -p $out
    cp -r cgen/* $out
  '';
  buildInputs = [
    SDL2
    elfkickers
  ];
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/img
    cp ${./img}/* $out/img
    cp simple-rts $out/bin
  '';
}
