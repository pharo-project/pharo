accessing
metacelloVersion
	| configuration |
	
	configuration := self configuration project.
	self version 
		ifNil: [ ^ configuration stableVersion ].
		
	"Now let's wrap Metcaello's strange behaviors of distinguishing strings and symbols"
	[ 
		"First we try to load the latest version for a given version string"
		^ (configuration latestVersion: self version)
			ifNil: [ 
				"If that doesn't work we use the default version lookup to find a version with the given name"
				configuration version: self version ] 
	] on: MetacelloVersionDoesNotExistError do: [ :error |
			^ self version = 'last'
				ifTrue: [
					"manual fallback since there is no symbolic name for lastVersion"
					configuration lastVersion ]
				ifFalse: [ 
					"symbols and strings are not equal in Meteacello..."
					configuration version: self version asSymbol ]].