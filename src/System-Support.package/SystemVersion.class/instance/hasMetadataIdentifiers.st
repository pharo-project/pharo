testing
hasMetadataIdentifiers
	"Return true if we have metadata identifiers (see rule 10 on semver.org) that need to be separated with a + sign"
	
	^self suffix notEmpty | self hasValidBuildNumber | self commitHash notEmpty 