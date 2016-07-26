instance creation
hierarchyOf: aClass definesVariable: aString 
	^self new 
		type: (Array 
				with: #hierarchyDefinesInstVar
				with: aClass
				with: aString)
		block: [aClass hierarchyDefinesVariable: aString]
		errorString: aClass printString 
				, ' or one of its subclasses <1?:does not >define<1?s:> variable ' 
					, aString