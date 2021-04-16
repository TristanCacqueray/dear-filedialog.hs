{ nixpkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/18cc4df312e74e6d47ea8d1c26f19a3d8f524ed7.tar.gz")
  { } }:
let
  name = "dear-filedialog";
  haskellPackages = nixpkgs.haskellPackages.extend (self: super: {
    dear-imgui = self.callCabal2nixWithOptions "dear-imgui"
      (nixpkgs.fetchFromGitHub {
        owner = "haskell-game";
        repo = "dear-imgui.hs";
        rev = "fad17edb069a1b2f8dc40e83b99681ccccb237c9";
        sha256 = "1kn1xxx46f4g59khj6b07lrkwv4amsld7w8sp2w34ak8krzk7sq6";
        fetchSubmodules = true;
      }) "--flag=-sdl --flag=glfw --flag=opengl3" { };
  });
  drv = haskellPackages.callCabal2nix name ./. { };
  shellDrv = haskellPackages.shellFor {
    withHoogle = false;
    packages = p: [ drv ];
    buildInputs = with haskellPackages; [
      nixpkgs.glfw3
      nixpkgs.glew
      nixpkgs.pkgconfig
      haskell-language-server
      hlint
      cabal-install
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
