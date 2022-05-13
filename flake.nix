{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages = with nixpkgs.legacyPackages."${system}"; rec {
        erlang_24 = erlang.overrideAttrs (oldAttrs: rec {
          name = "erlang-${version}";
          version = "24.3";
          # nix-prefetch-url --unpack https://github.com/erlang/otp/archive/refs/tags/OTP-24.3.zip
          src = fetchFromGitHub {
            owner = "erlang";
            repo = "otp";
            rev = "OTP-${version}";
            sha256 = "0dm6b5cwgw4vn344hk8jchk35m2ijgwbvc14lgbbazrbmd5xiyaf";
          };

          configureFlags = oldAttrs.configureFlags
            ++ [ "--with-ssl=${lib.getOutput "out" openssl}" ]
            ++ [ "--with-ssl-incl=${lib.getDev openssl}" ];
        });

        elixir_1_13 = elixir.overrideAttrs (oldAttrs: rec {
          name = "elixir-${version}";
          version = "1.13.2";

          buildInputs = [ erlang_24 ];

          # nix-prefetch-url --unpack https://github.com/elixir-lang/elixir/archive/refs/tags/v1.13.4.tar.gz

          src = fetchFromGitHub {
            owner = "elixir-lang";
            repo = "elixir";
            rev = "v${version}";
            sha256 = "1z19hwnv7czmg3p56hdk935gqxig3x7z78yxckh8fs1kdkmslqn4";
          };

          nativeBuildInputs = oldAttrs.nativeBuildInputs or [ ]
            ++ [ makeWrapper ];
        });
      };

      devShell = with nixpkgs.legacyPackages."${system}";
        mkShell {
          buildInputs = with self.packages."${system}"; [
            erlang_24
            elixir_1_13
          ];
        };
    });
}
