system integration
systemCategoryAddedActionFrom: ann
	| package |
	
	package := self packageMatchingExtensionName: ann categoryName asString.
	package ifNil: [ 
		package := self registerPackage: (self packageClass named: ann categoryName asSymbol) ].
	package addClassTag: ann categoryName asSymbol.