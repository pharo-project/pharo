private-extensions-traitdescription
duringTestCompileSilentlyForTraitDescription: code classified: aCategory
	<flExtensionOf: #TraitDescription selector: #duringTestCompileSilently:classified:>
	^ self 
		duringTestCompileSilently: code
		storeSource: true
		classified: aCategory