running
runPackage: aString
	^ self runClasses: (RPackage organizer packageNamed: aString) definedClasses named: aString