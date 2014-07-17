require("RUnit")
require("netbiov")

pattern="^test_.*\\.R$"
runitDirs <- c(".")
TEST_DATA_DIR <- "data"
BiocGenerics:::testPackage("netbiov")
suite <- defineTestSuite(name="NetBioV Suite",
                         dirs=runitDirs,
                         testFileRegexp=pattern,
                         rngKind="default",
                         rngNormalKind="default")
result <- runTestSuite(suite)
printTextProtocol(result)
