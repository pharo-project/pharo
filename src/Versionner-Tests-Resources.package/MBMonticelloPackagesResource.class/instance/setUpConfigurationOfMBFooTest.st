ConfigurationOfMBFooTest
setUpConfigurationOfMBFooTest
	"self reset"  
	| versionInfo |
	versionInfo := self setUpConfigurationOfMBFooTest1.
	versionInfo := self setUpConfigurationOfMBFooTest2: { versionInfo }.
	versionInfo := self setUpConfigurationOfMBFooTest3: { versionInfo }.
