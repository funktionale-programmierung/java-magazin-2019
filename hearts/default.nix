{ mkDerivation, base, mtl, random, stdenv, transformers, free }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl random transformers free ];
  homepage = "https://github.com/peterthiemann/hearts#readme";
  license = stdenv.lib.licenses.bsd3;
}
