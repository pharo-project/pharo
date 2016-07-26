querying
packageNamed: aString

	^self packageNamed: aString ifAbsent: [ ^nil ].