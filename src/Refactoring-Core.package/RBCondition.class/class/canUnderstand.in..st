instance creation
canUnderstand: aSelector in: aClass
	^self new
		type: (Array with: #understandsSelector with: aClass with: aSelector)
		block: [aClass canUnderstand: aSelector]
		errorString: aClass printString , ' <1?:does not >understand<1?s:> ' , aSelector printString