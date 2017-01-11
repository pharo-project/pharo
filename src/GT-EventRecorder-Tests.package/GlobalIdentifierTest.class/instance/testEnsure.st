tests
testEnsure
	"The computer UUID is not stored immediately because the Pharo Integration Server
	raised building errors when storing the computer UUID on the CI server."
	| stored |
	self testPreferenceFilesNotCollideWithRealPreferences.
	identifier ensure.
	self assert: preferences exists description: 'The actual preferences should be stored now.'.
	stored := identifier persistenceStrategy load.
	self assert: stored equals: identifier persistedInformation.
