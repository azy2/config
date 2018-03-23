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
  shellHook = ''
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    pyenv activate ovs
    export FLASK_APP=main.py
    export FLASK_DEBUG=True
  '';
}
