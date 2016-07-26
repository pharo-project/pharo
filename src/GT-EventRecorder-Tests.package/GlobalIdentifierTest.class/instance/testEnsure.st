tests
testEnsure
	"The computer UUID is not stored immediately because the Pharo Integration Server
	raised building errors when storing the computer UUID on the CI server."
	| stored |
	identifier ensure.
	self assert: preferences exists description: 'The preferences should be stored now.'.
	stored := identifier persistenceStrategy load.
	self assert: stored equals: identifier persistedInformation.