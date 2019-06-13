{ mkDerivation, base, mtl, random, stdenv, transformers, free, cond }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl random transformers free cond ];
  homepage = "https://github.com/peterthiemann/hearts#readme";
  license = stdenv.lib.licenses.bsd3;
}
