comparing
< aMetacelloVersion

	aMetacelloVersion species = self species ifFalse: [ ^false ].
	^self versionNumber < aMetacelloVersion versionNumber