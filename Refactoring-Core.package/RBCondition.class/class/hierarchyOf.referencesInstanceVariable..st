instance creation
hierarchyOf: aClass referencesInstanceVariable: aString 
	^self new 
		type: (Array 
				with: #hierarchyReferencesInstVar
				with: aClass
				with: aString)
		block: 
			[(aClass withAllSubclasses 
				detect: [:each | (each whichSelectorsReferToInstanceVariable: aString) isEmpty not]
				ifNone: [nil]) notNil]
		errorString: aClass printString 
				, ' or subclass <1?:does not >reference<1?s:> instance variable ' , aString