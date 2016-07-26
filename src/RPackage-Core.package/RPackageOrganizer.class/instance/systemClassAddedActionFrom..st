system integration
systemClassAddedActionFrom: ann
	| class rPackage categoryNameSymbol |
	
	class := ann classAffected.

	categoryNameSymbol := class category.
	rPackage := (self packageMatchingExtensionName: categoryNameSymbol) 
		ifNil: [ self registerPackage: (self packageClass named: categoryNameSymbol) ].		
	
	rPackage importClass: class.
