instance creation
definesClassVariable: aString in: aClass 
	^self new 
		type: (Array 
				with: #definesClassVar
				with: aClass
				with: aString)
		block: [aClass definesClassVariable: aString]
		errorString: aClass printString 
				, ' <1?:does not >define<1?s:> class variable ' , aString