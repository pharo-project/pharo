running
runCategory: aString
	^ self runClasses: (Smalltalk organization classesInCategory: aString) named: aString