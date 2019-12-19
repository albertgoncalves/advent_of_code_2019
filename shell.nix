with import <nixpkgs> {};
let
    ocamlPackages = ocaml-ng.ocamlPackages_4_09.overrideScope' (self: super: {
        ocaml = super.ocaml.override {
            flambdaSupport = false;
        };
    });
in
let
    shared = [
        (with ocamlPackages; [
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
