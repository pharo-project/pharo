initialization-data
initializeFor: aBehavior
	| package |
		
	package := self packageMatchingExtensionName: aBehavior category.
	package ifNil: [ 
		"It should not happen. 
		 But actually could happen that one class is in a SystemCategory and not in a MC"
		package := self basicRegisterPackage: (RPackage named: aBehavior category) ]. 
	package addClassDefinition: aBehavior.
	package 
		addClassDefinition: aBehavior 
		toClassTag: aBehavior category asSymbol.
	self registerPackage: package forClass: aBehavior.