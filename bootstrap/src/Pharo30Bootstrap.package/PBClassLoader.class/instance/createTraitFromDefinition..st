class-loading
createTraitFromDefinition: aTrait

	| newTrait |	
	newTrait := builder objectSpace interpreter evaluateCode: aTrait definition.
	self registerTrait: newTrait named: aTrait name.
	^ newTrait