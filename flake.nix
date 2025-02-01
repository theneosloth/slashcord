{
  inputs = { utils.url = "github:numtide/flake-utils"; };
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              pkgs.zrok
              (sbcl.withPackages (ps:
                with ps; [
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
                ]))
            ];
        };
      });
}
