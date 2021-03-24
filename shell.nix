let np = import <nixpkgs> {};
in np.mkShell { buildInputs = [np.gperftools np.haskell.compiler.ghc8102]; }
