{ mkDerivation, base, brick, req, stdenv, vty }:
mkDerivation {
  pname = "hurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base brick req vty ];
  executableHaskellDepends = [ base brick vty ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
