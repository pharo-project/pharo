visiting
visitMCProject: aProject withVersion: aVersion
	"visit a Metacello Project with the specified version."
	| version |
	
	version := MTVersion fromVersionString: aVersion versionString.
	project version: version.
	aVersion ifNotNil: [ self visitMCVersion: aVersion ].