ConfigurationOfMBBranchTest
setUpConfigurationOfMBBranchTest
	"self reset"  
	| versionInfo |
	versionInfo := self setUpConfigurationOfMBBranchTest1.
	versionInfo := self setUpConfigurationOfMBBranchTest2: { versionInfo }.
