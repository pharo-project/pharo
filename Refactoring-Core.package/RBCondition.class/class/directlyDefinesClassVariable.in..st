instance creation
directlyDefinesClassVariable: aString in: aClass 
	^self new 
		type: (Array with: #directlyDefinesClassVar with: aClass with: aString)
		block: [aClass directlyDefinesClassVariable: aString]
		errorString: aClass printString 
				, ' <1?:does not >directly define<1?s:> class variable ' , aString