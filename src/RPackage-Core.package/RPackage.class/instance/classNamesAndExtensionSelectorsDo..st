slices
classNamesAndExtensionSelectorsDo: aBlock
	"Iterate over the extended methods grouped by classes and apply the argument.
	The block will be passed a class name and each individual selectors.
	Only classes with methods are paired with their methods"
	
	classExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
			methods do: [ :sel |  aBlock value: classSymbol	value: sel]].
	
	metaclassExtensionSelectors keysAndValuesDo: [ :classSymbol :methods |
			methods do: [ :sel |  aBlock 
									value: (classSymbol, '_class') asSymbol
									value: sel]].	