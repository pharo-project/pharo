accessing
ensureCategoryByName: aCategorySymbol
	| category |
	(self includesCategoryNamed: aCategorySymbol)
		ifFalse: [ 
			category := KMCategory named: aCategorySymbol.
			self addCategory: category ]
		ifTrue: [ category := self categoryForName: aCategorySymbol ].
	^ category