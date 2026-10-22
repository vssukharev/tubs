{ 
  mkShell,
  lib,
  scala,
  sbt,
  zulu25,
  libGL,
  libxkbcommon,
  libX11,
  libXxf86vm,
  libXtst,
}: 
let 
  graphics = [
    libGL
    libxkbcommon
    libX11
    libXxf86vm
    libXtst
  ];
  
in mkShell {
  buildInputs = lib.lists.flatten [
    ( sbt.override { jre = zulu25; } )
    zulu25
    graphics
  ];

  LD_LIBRARY_PATH = lib.makeLibraryPath graphics;
  JAVA_TOOL_OPTIONS = "--enable-native-access=ALL-UNNAMED";
}

