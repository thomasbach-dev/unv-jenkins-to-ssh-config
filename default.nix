{ mkDerivation, aeson, base, bytestring, connection, exceptions
, filepath, hspec, http-client, http-client-tls, lib
, optparse-applicative, parsec, pretty-show, unordered-containers
, yaml
}:
mkDerivation {
  pname = "jenkins-to-ssh-config";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring connection exceptions filepath http-client
    http-client-tls optparse-applicative parsec pretty-show
    unordered-containers yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring connection exceptions filepath http-client
    http-client-tls optparse-applicative parsec pretty-show
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base bytestring connection exceptions filepath hspec
    http-client http-client-tls optparse-applicative parsec pretty-show
    unordered-containers yaml
  ];
  homepage = "https://github.com/thomasbach-dev/unv-jenkins-to-ssh-config";
  description = "Helpers to help porting stuff at Univention";
  license = lib.licenses.bsd3;
  mainProgram = "jenkins-to-ssh-config";
}
