private-extensions-traitdescription
duringTestCompileSilentlyForTraitDescription: code
	<flExtensionOf: #TraitDescription selector: #duringTestCompileSilently:>
	^ self 
		duringTestCompileSilently: code 
		storeSource: true