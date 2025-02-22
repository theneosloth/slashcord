{
  inputs = { utils.url = "github:numtide/flake-utils"; };
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lispDeps = with pkgs; [
            sbclPackages.alexandria
            sbclPackages.ironclad
            sbclPackages.hunchentoot
            sbclPackages.easy-routes
            sbclPackages.clj-arrows
            sbclPackages.json-mop
            sbclPackages.yason
            sbclPackages.serapeum
            sbclPackages.fiveam
            sbclPackages.dexador
            sbclPackages.log4cl
            sbclPackages.access
        ];


        sbcl' = (pkgs.sbcl.withPackages (ps: lispDeps));

        slashcord = pkgs.sbcl.buildASDFSystem rec {
          pname = "slashcord";
          version = "unstable";
          systems = ["slashcord"];
          src = ./.;

          nativeBuildInputs = with pkgs; [];
          nativeLibs = with pkgs; [];
          lispLibs = lispDeps;
        };
  in {
    devShell = pkgs.mkShell {
      buildInputs = with pkgs;
        [
          pkgs.zrok
          sbcl'
        ];
    };
    packages.default = slashcord;
  });
}
