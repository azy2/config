with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    python36Full
    python36Packages.pip
    python36Packages.virtualenv
    gcc7
    libffi
    openssl
  ];
  shellHook = ''
    sudo service mysql start
    mysql -u root -ppass123 -e "DROP DATABASE IF EXISTS ovs; CREATE DATABASE ovs;"
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    pyenv activate ovs
    export FLASK_APP=main.py
    export FLASK_DEBUG=True
    pip install -r requirements.txt
    cd database
    alembic upgrade head
    cd ..
  '';
}
