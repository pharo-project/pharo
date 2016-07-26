private-extensions-traitcomposition
remove: aTransformation
	<flExtensionOf: #TraitComposition>
	aTransformation ifNil: [ ^ self ].
	
	self transformations
		remove: aTransformation