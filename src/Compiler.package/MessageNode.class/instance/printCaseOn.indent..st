printing
printCaseOn: aStream indent: level 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printOn: aStream indent: level]
		ifFalse: 
			[aStream nextPutAll: '{'; crtab: level + 1.
			 braceNode casesForwardDo:
				[:keyNode :valueNode :last | 
				keyNode printOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 crtab: level + 1]]].
	otherwise notNil ifTrue:
		[aStream crtab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise isComplex
					ifTrue: 
						[aStream crtab: level + 2.
						 1]
					ifFalse: [0].
		 otherwise printOn: aStream indent: level + 1 + extra]