with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    python36Full
    python36Packages.pip
    python36Packages.virtualenv
    mysql57
    gcc7
    libffi
    openssl
  ];
}
