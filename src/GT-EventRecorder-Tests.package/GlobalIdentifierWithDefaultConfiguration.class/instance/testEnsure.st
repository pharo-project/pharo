tests
testEnsure
	| stored |
	super testEnsure.
	self assert: oldPreferences exists description: 'The old preferences should be stored now.'.
	stored := identifier persistenceStrategy previous load.
	self assert: stored equals: identifier persistedInformation.