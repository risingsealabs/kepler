{}:
let project = (import ./project.nix {});
in project.nixpkgs.haskellPackages.shellFor {
  withHoogle = true;
  packages = p: builtins.attrValues (project.keplerPackages' p);
  nativeBuildInputs = builtins.attrValues project.buildInputs;
}
