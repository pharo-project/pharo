instance creation
hierarchyOf: aClass canUnderstand: aSelector
	^self new
		type: (Array with: #hierarchyUnderstandsSelector with: aClass with: aSelector)
		block: [aClass hierarchyDefinesMethod: aSelector]
		errorString: aClass printString , ' <1?or a subclass:and all subclasses do not> understand<1?s:> ' , aSelector printString