visitor
visitTraitModification: aChange

	self compiler evaluate: aChange newTrait definitionSource
