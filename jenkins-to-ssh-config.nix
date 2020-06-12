{ mkDerivation, base, bytestring, connection, exceptions, filepath
, hspec, http-client, http-client-tls, optparse-applicative, parsec
, stdenv, unordered-containers, yaml
}:
mkDerivation {
  pname = "jenkins-to-ssh-config";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring connection exceptions filepath http-client
    http-client-tls parsec unordered-containers yaml
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base bytestring hspec parsec ];
  homepage = "thomasbach.dev";
  description = "Helpers to help porting stuff at Univention";
  license = stdenv.lib.licenses.bsd3;
}
