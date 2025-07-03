{
  inputs = { utils.url = "github:numtide/flake-utils"; };
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nativeLibs = with pkgs; [ openssl ];
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
        src = ./.;

        version = "unstable";
        pname = "slashcord";

        lispLibs = with pkgs; [
          sbclPackages.alexandria
          sbclPackages.ironclad
          sbclPackages.hunchentoot
          sbclPackages.easy-routes
          sbclPackages.json-mop
          sbclPackages.yason
          sbclPackages.serapeum
          sbclPackages.fiveam
          sbclPackages.dexador
        ];

        slashcord = pkgs.sbcl.buildASDFSystem {
          inherit pname version src nativeLibs lispLibs;
          systems = [ "slashcord" ];
        };

        sbcl' = pkgs.sbcl.withOverrides (self: super: { inherit slashcord; });

        lisp = sbcl'.withPackages (ps: [ ps.slashcord ]);

        slashcord-bin = pkgs.stdenv.mkDerivation {
          inherit src version;
          pname = "slashcord-bin";
          nativeBuildInputs = [ sbcl' pkgs.makeWrapper ];
          dontStrip = true;
          buildPhase = ''
            ${lisp}/bin/sbcl --noinform --disable-debugger <<EOF
              (declaim (optimize (speed 3) (debug 0)))
              (load (sb-ext:posix-getenv "ASDF"))
              (asdf:load-system :slashcord)
              (setf uiop:*image-entry-point* #'slashcord/server:main)
              (uiop:dump-image "slashcord"
                :executable t
                #+sb-core-compression :compression
                #+sb-core-compression t)"
            EOF
          '';

          installPhase = ''
            install -D slashcord $out/bin/slashcord
          '';

          postFixup = ''
            wrapProgram $out/bin/slashcord \
                 --prefix LD_LIBRARY_PATH : ${LD_LIBRARY_PATH}
          '';

        };

        slashcord-main = pkgs.writeShellScriptBin "slashcord" ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          exec ${lisp}/bin/sbcl \
          --noinform \
          --disable-debugger \
          --eval '(declaim (optimize (speed 3) (debug 0)))' \
          --eval '(load (sb-ext:posix-getenv "ASDF"))' \
          --eval "(asdf:load-system :slashcord)" \
          --eval '(slashcord/server::main)'
          "$@"
        '';

      in rec {
        devShells.default = pkgs.mkShell {
          shellHook = ''
            export CL_SOURCE_REGISTRY=$PWD
          '';
          buildInputs = with pkgs; [
            cloudflared
            (sbcl.withPackages (ps: lispLibs))
          ];
        };

        packages = {
          default = slashcord-main;
          binary = slashcord-bin;
          dockerImage = pkgs.dockerTools.buildImage {
            name = "neosloth/slashcord";
            tag = "latest";
            created = "now";
            # SBCL can't run if there's no user configured in the image
            runAsRoot = ''
              ${pkgs.dockerTools.shadowSetup}
              groupadd -r slashcord
              useradd -r -g slashcord slashcord
              mkdir /opt
              chown slashcord:slashcord /opt
            '';
            config = {
              Cmd = [ "${self.packages.${system}.binary}/bin/slashcord" ];
              WorkingDir = "/opt";
            };
          };
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/slashcord";
        };

      });
}
