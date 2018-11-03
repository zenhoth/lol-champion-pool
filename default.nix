{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , directory, HTTP, lens-aeson, mtl, stdenv, xdg-basedir
      }:
      mkDerivation {
        pname = "lol-champion-pool";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring containers directory HTTP lens-aeson mtl
          xdg-basedir
        ];
        executableHaskellDepends = [ base directory mtl xdg-basedir ];
        homepage = "https://github.com/zenhoth/lol-champion-pool#readme";
        description = "A tool that uses the champion.gg API to calculate assorted information surrounding your champion pool in League of Legends";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
