instance creation
definesInstanceVariable: aString in: aClass 
	^self new 
		type: (Array 
				with: #definesInstVar
				with: aClass
				with: aString)
		block: [aClass definesInstanceVariable: aString]
		errorString: aClass printString 
				, ' <1?:does not >define<1?s:> instance variable ' , aString