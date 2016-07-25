testing
includesKeymapCategory: aCategoryName targetting: anObject

	^self targets anySatisfy: [ :tgt | 
		tgt target = anObject and: [ tgt category name = aCategoryName ] ]
		