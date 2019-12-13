with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_07; [
            ocaml
            ocp-indent
        ])
        rlwrap
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
