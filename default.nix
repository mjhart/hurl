{ mkDerivation, aeson, base, brick, bytestring, case-insensitive
, exceptions, http-conduit, http-types, req, stdenv, text
, unordered-containers, vector, vty
}:
mkDerivation {
  pname = "hurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base brick bytestring case-insensitive exceptions
    http-conduit http-types req text unordered-containers vector vty
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/mjhart/hurl";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
