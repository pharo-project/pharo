tests
test002ConfigurationReferences
	"Make sure that the project and project branches show up in configuration references (for load configuration command)"

	| referenceMap |
	referenceMap := MBLoadConfigurationCommand new configurationReferencesFrom: MBMonticelloPackagesResource current monticelloRepository.
	self assert: (referenceMap keys includes: 'ConfigurationOfMBBranchTests.branch')