{
  description = "HexSpace - A Haskell Libary for hex based games";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        packageName = "hexspace";
        
        # Create the package with explicit dependencies
        package = pkgs.haskellPackages.callCabal2nix packageName ./. { } ;
        
      in {
        packages.${packageName} = package;
        packages.default = package;
        
        devShells.default = pkgs.mkShell {
          # Add both Haskell packages and native libraries
          buildInputs = with pkgs; [
            aider-chat
            # Haskell development tools
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            
            # Required native libraries for OpenGL/Gloss
            pkg-config
            libGL
            libGLU
            freeglut

	    # Required by JuicyPizels
            zlib
            
            # X11 dependencies
            xorg.libX11
            xorg.libXrandr
            xorg.libXi
            xorg.libXinerama
            xorg.libXcursor
            xorg.libXxf86vm
          ];
          
          # Set library paths for both build-time and runtime
          shellHook = ''
            export PKG_CONFIG_PATH=${pkgs.libGLU}/lib/pkgconfig:${pkgs.libGL}/lib/pkgconfig:$PKG_CONFIG_PATH
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [
              pkgs.libGL
              pkgs.libGLU
              pkgs.freeglut
              pkgs.xorg.libX11
              pkgs.xorg.libXrandr
              pkgs.xorg.libXinerama
              pkgs.xorg.libXcursor
              pkgs.xorg.libXi
              pkgs.xorg.libXxf86vm
            ]}
          '';
        };
      }
    );
}
