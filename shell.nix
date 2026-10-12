{ 
  mkShell,
  scala,
  sbt,
}: 
mkShell {
  buildInputs = [
    sbt
  ];
}

