{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (final: prev: {
      osHelpers = {
        wrapPackage = package: args:
          pkgs.stdenvNoCC.mkDerivation {
            name = package.name + "-wrapped";
            src = package;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            dontBuild = true;
            preferLocalBuild = true;
            allowSubstitutes = false;
            installPhase = ''
              for bin in $(ls $src/bin); do
                makeWrapper $src/bin/$bin $out/bin/$bin ${args}
              done
            '';
          };
      };
    })
  ];
}
