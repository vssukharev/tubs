{ 
  mkShell,
  lib,
  scala,
  sbt,
  zulu25,
  glib,
  gsettings-desktop-schemas,
  gtk3,
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
  nativeBuildInputs = [
    glib
    gsettings-desktop-schemas
  ];

  buildInputs = lib.lists.flatten [
    ( sbt.override { jre = zulu25; } )
    gtk3
    zulu25
    graphics
  ];

  LD_LIBRARY_PATH = lib.makeLibraryPath graphics;
  JAVA_TOOL_OPTIONS = "--enable-native-access=ALL-UNNAMED";
  GSETTINGS_SCHEMA_DIR = "${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}/glib-2.0/schemas:${gtk3}/share/gsettings-schemas/${gtk3.name}/glib-2.0/schemas";
}

