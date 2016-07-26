building
detachKeymapCategory: aCategoryName targetting: anObject
	self targets
		detect: [ :tgt | tgt target = anObject and: [ tgt category name = aCategoryName ] ]
		ifFound: [ :categoryTarget | self targets remove: categoryTarget ]
		ifNone: [ self error: 'Category ' , aCategoryName , ' is not attached to ' , morph asString ]