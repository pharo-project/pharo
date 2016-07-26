private-extensions-traitdescription
duringTestCompileSilentlyForTraitDescription: code storeSource: storeSource
	<flExtensionOf: #TraitDescription selector: #duringTestCompileSilently:storeSource:>
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: ''