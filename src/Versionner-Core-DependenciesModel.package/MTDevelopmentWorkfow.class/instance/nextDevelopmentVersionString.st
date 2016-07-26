protocol
nextDevelopmentVersionString
	| newVersionString |
	
	newVersionString := self developmentVersion versionString.
	[ project configurationClass project hasVersion: newVersionString ] whileTrue: [
		newVersionString := self nextDevelopmentVersionString: newVersionString ].
	^ newVersionString