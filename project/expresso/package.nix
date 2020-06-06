{ mkDerivation, base, containers, directory, fetchgit, filepath
, hashable, haskeline, mtl, parsec, stdenv, tasty, tasty-hunit
, template-haskell, text, unordered-containers, wl-pprint
}:
mkDerivation {
  pname = "expresso";
  version = "0.1.2.2";
  /* src = fetchgit {
    url = "https://github.com/willtim/Expresso.git";
    sha256 = "1npzp8x5bf1vrzwr6jwzk6ppbg79pkmnfmh8nw6wrm3q2ibjpmy0";
    rev = "add7480c867b98819a482adb6bcd2da730928f1a";
    fetchSubmodules = true;
  }; */
  src = ../../../Expresso;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base containers directory filepath hashable haskeline mtl parsec
    template-haskell text unordered-containers wl-pprint
  ];
  executableHaskellDepends = [
    base containers directory filepath hashable haskeline mtl parsec
    text unordered-containers wl-pprint
  ];
  testHaskellDepends = [
    base containers directory filepath hashable haskeline mtl parsec
    tasty tasty-hunit text unordered-containers wl-pprint
  ];
  description = "A simple expressions language based on row types";
  license = stdenv.lib.licenses.bsd3;
}
