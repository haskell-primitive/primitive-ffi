{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  primitive-ffi = (
    with rec {
      primitive-ffiSource = pkgs.lib.cleanSource ../.;
      primitive-ffiBasic  = self.callCabal2nix "primitive-ffi" primitive-ffiSource { };
    };
    overrideCabal primitive-ffiBasic (old: {
    })
  );
}
