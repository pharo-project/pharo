instance creation
definesSelector: aSelector in: aClass 
	^self new
		type: (Array with: #definesSelector with: aClass with: aSelector)
		block: [aClass directlyDefinesMethod: aSelector]
		errorString: aClass printString , ' <1?:does not >define<1?s:> ' , aSelector printString