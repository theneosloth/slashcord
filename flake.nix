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
                  sbclPackages.lispcord
                  sbclPackages.ironclad
                  sbclPackages.flexi-streams
                  sbclPackages.ningle
                  sbclPackages.serapeum
                  sbclPackages.com_dot_inuoe_dot_jzon
                  sbclPackages.hunchentoot
                  sbclPackages.easy-routes
                ]))
            ];
        };
      });
}
