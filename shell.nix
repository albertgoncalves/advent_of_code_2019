with import <nixpkgs> {};
let
    shared = [
        (with ocaml-ng.ocamlPackages_4_07; [
            ocaml
            ocp-indent
        ])
        rlwrap
        rustup
        shellcheck
    ];
    hook = ''
        . .shellhook
    '';
in
{
    darwin = mkShell {
        buildInputs = shared;
        shellHook = hook;
    };
    linux = gccStdenv.mkDerivation {
        name = "_";
        buildInputs = [
            pkg-config
        ] ++ shared;
        shellHook = hook;
    };
}
