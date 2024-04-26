{ }:
let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/4c86138ce486d601d956a165e2f7a0fc029a03c1.tar.gz;
    sha256 = "sha256:0bw84ndw6f82yapkkpqnqbv1izyys3y79nlmg04576z53ccggjgb";
  }) { inherit overlays; };

  gitignore = nixpkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  gitignoreSource = (import gitignore {}).gitignoreSource;

  iavl = nixpkgs.callPackage ./iavl.nix {};

  tendermint = (nixpkgs.callPackage ./tendermint.nix {}).overrideAttrs { __darwinAllowLocalNetworking = true; };

  root = ../.;

  packages = {
    hs-abci-extra = root + /hs-abci-extra;
    hs-abci-sdk = root + /hs-abci-sdk;
    hs-abci-server = root + /hs-abci-server;
    hs-abci-test-utils = root + /hs-abci-test-utils;
    hs-abci-types = root + /hs-abci-types;
    hs-iavl-client = root + /hs-iavl-client;
    hs-tendermint-client = root + /hs-tendermint-client;
    nameservice = root + /hs-abci-docs/nameservice;
    simple-storage = root + /hs-abci-docs/simple-storage;
  };

  repos = {
    avl-auth = nixpkgs.fetchFromGitHub {
      owner  = "oscoin";
      repo   = "avl-auth";
      rev    = "dfc468845a82cdd7d759943b20853999bc026505";
      sha256 = "005j98hmzzh9ybd8wb073i47nwvv1hfh844vv4kflba3m8d75d80";
    };

    # https://github.com/haskell-grpc-native/http2-client/pull/92#issuecomment-2079254415
    http2-client = nixpkgs.fetchFromGitHub {
      owner  = "haskell-grpc-native";
      repo   = "http2-client";
      rev    = "0140d8fc21e240325a19da4ce641c346df5f5079";
      sha256 = "sha256-umw1EJ9NCfu5LrAbD/s4IKFFtUhIkIt4MwrOfedMf/0=";
    };

    http2-grpc-haskell = nixpkgs.fetchFromGitHub {
      owner  = "haskell-grpc-native";
      repo   = "http2-grpc-haskell";
      rev    = "7c19009e37fc305f73988b06b2cba9f31ae5478e";
      sha256 = "sha256-aDn9LsImlz2mSbieSsW+42e8Rsv8oEgSv6MKXsntArk=";
    };

    # https://github.com/well-typed/large-records/pull/151
    large-records = nixpkgs.fetchFromGitHub {
      owner  = "TristanCacqueray";
      repo   = "large-records";
      rev    = "7576d3de071d775901073ab4c714b984fab84c95";
      sha256 = "sha256-kKF81qs93I/Wj7Ah+bneFYFz8xIulT2E85CIJuq3zf4=";
    };
  };

  extra-build-inputs = with nixpkgs; {
    hs-abci-sdk = [protobuf];
    hs-abci-types = [protobuf];
    hs-iavl-client = [protobuf];
    simple-storage = [protobuf];
  };

  addBuildInputs = inputs: { buildInputs ? [], ... }: { buildInputs = inputs ++ buildInputs; };

  # TODO: figure out why running tests on mac causes builds to hang _after_ running the tests
  keplerTests = pkg: args: if nixpkgs.stdenv.isDarwin then nixpkgs.haskell.lib.dontCheck pkg else keplerTests' pkg args;

  keplerTests' = pkg: { runIavl ? false, runABCI ? null, runTendermint ? null }: nixpkgs.lib.overrideDerivation pkg (drv:
    let
      iavlScript = ''
        ${iavl}/bin/iavlserver -db-name "test" -datadir "." -grpc-endpoint "0.0.0.0:8090" -gateway-endpoint "0.0.0.0:8091" &
        sleep 3
      '';
      abciScript = ''
        ${runABCI} &
        sleep 3
      '';
      tendermintScript = ''
        ${tendermint}/bin/tendermint init --home $TMPDIR
        ${tendermint}/bin/tendermint node --home $TMPDIR --proxy_app=${runTendermint} &
        sleep 3
      '';
    in {
      checkPhase = with nixpkgs.lib; concatStrings [
        (optionalString runIavl iavlScript)
        (optionalString (runABCI != null) abciScript)
        (optionalString (runTendermint != null) tendermintScript)
        drv.checkPhase
      ];
    });

  haskellOverrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
    (self: super: builtins.mapAttrs
      (name: path: (self.callCabal2nix name (gitignoreSource path) {}).overrideAttrs (addBuildInputs (extra-build-inputs.${name} or [])))
      packages
    )
    (self: super: with nixpkgs.haskell.lib; {
      avl-auth = dontCheck (self.callCabal2nix "avl-auth" repos.avl-auth {});  # https://github.com/haskell-haskey/xxhash-ffi/issues/2

      http2-client = doJailbreak (self.callCabal2nix "http2-client" repos.http2-client {});
      http2-client-grpc = doJailbreak (self.callCabal2nixWithOptions "http2-client-grpc" repos.http2-grpc-haskell "--subpath http2-client-grpc" {});

      http2-grpc-proto-lens = doJailbreak (unmarkBroken super.http2-grpc-proto-lens);
      http2-grpc-types = doJailbreak (unmarkBroken super.http2-grpc-types);
      large-generics = doJailbreak (unmarkBroken super.large-generics);
      large-records = doJailbreak (self.callCabal2nixWithOptions "large-records" repos.large-records "--subpath large-records" {});

      proto-lens-arbitrary = unmarkBroken super.proto-lens-arbitrary;
      proto3-suite = dontCheck (doJailbreak super.proto3-suite); # Test suite fails to build with module discovery errors
      proto3-wire = doJailbreak (unmarkBroken super.proto3-wire);
    })
  ];

  overlay = self: super: {
    inherit iavl tendermint;

    haskellPackages =
      super.haskellPackages.override (_old: {
        overrides = haskellOverrides;
    });
  };

  overlays = [overlay];

in rec {
  inherit nixpkgs overlays;

  buildInputs = {
    inherit (nixpkgs) /*iavl*/ protobuf tendermint;
    inherit (nixpkgs.haskellPackages) cabal-install ghcid hlint stack stylish-haskell weeder;
  };

  keplerPackages = keplerPackages' nixpkgs.haskellPackages;
  keplerPackages' = p: with p; {
    inherit
      hs-abci-extra
      hs-abci-sdk
      hs-abci-server
      hs-abci-test-utils
      hs-abci-types
      hs-iavl-client
      hs-tendermint-client
      nameservice
      simple-storage
    ;
  };
}
