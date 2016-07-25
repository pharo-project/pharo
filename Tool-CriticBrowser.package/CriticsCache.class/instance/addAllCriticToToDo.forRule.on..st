private
addAllCriticToToDo: aCollectionOfCritic forRule: aRule on: aManifestBuilder

	| ruleId versionId  |
	ruleId := aRule class uniqueIdentifierName.
	versionId := aRule class identifierMinorVersionNumber.
	(aManifestBuilder hasToDoOf: ruleId version: versionId) 
		ifFalse: [ aManifestBuilder installToDoOf: ruleId version: versionId ].		
	 aManifestBuilder addAllToDo: aCollectionOfCritic of: ruleId version: versionId.

	