private
addAllCriticToFalsePositive: aCollectionOfCritic forRule: aRule on: aManifestBuilder

	| ruleId versionId  criticss |
	criticss := aCollectionOfCritic reject: [ :c | falsePositiveClasses includes: c criticTheNonMetaclassClass ].
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasFalsePositiveOf: ruleId version: versionId) 
		ifFalse: [ aManifestBuilder installFalsePositiveOf: ruleId version: versionId ].		
	 aManifestBuilder addAllFalsePositive: criticss of: ruleId version: versionId.

	