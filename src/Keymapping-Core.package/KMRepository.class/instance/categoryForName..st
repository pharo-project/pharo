accessing
categoryForName: aCategorySymbol
	^ self categories at: aCategorySymbol ifAbsentPut: [ KMCategory named: aCategorySymbol ]