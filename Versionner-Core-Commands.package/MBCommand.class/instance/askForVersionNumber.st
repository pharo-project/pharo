versions
askForVersionNumber
	"Return a new version number as a string"

	| latestVersion newVersion versions |
	newVersion := (versions := target configurationInfo versions) isEmpty
		ifTrue: [ '1.0' ]
		ifFalse: [ 
			latestVersion := versions first version.
			latestVersion versionNumber copy incrementMinorVersionNumber printString ].
	^ self uiManager  request: 'enter new version number' initialAnswer: newVersion