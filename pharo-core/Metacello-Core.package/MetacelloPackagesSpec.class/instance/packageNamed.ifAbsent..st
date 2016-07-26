accessing
packageNamed: aString ifAbsent: aBlock

	^self map at: aString ifAbsent: aBlock