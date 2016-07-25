accessing
sourceCode
	^self selectedClass sourceCodeAt: self selector ifAbsent: [ '' ].