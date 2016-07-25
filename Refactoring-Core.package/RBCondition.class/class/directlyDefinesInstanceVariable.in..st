instance creation
directlyDefinesInstanceVariable: aString in: aClass 
	^self new 
		type: (Array with: #directlyDefinesInstanceVariable with: aClass with: aString)
		block: [aClass directlyDefinesInstanceVariable: aString]
		errorString: aClass printString 
				, ' <1?:does not >directly define<1?s:> instance variable ' , aString