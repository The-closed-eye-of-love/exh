{ mkDerivation, aeson, base, bytestring, containers, exceptions
, HUnit, inline-c, mtl, QuickCheck, scientific, stdenv, string-conv
, tasty, tasty-hunit, tasty-quickcheck, text, time, transformers
, unliftio-core, unordered-containers, vector
}:
mkDerivation {
  pname = "quickjs-hs";
  version = "0.1.2.4";
  sha256 = "a7dbddee2106ed07fdf9e9265080950a16178615374a64bc648ba47dd0f569a1";
  libraryHaskellDepends = [
    aeson base bytestring containers exceptions inline-c mtl scientific
    string-conv text time transformers unliftio-core
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base exceptions HUnit QuickCheck tasty tasty-hunit
    tasty-quickcheck text unordered-containers vector
  ];
  homepage = "https://github.com/goodlyrottenapple/quickjs-hs#readme";
  description = "Wrapper for the QuickJS Javascript Engine";
  license = stdenv.lib.licenses.mit;
}
