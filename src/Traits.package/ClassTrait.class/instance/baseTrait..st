accessing parallel hierarchy
baseTrait: aTrait

	<localClassTraitMethod> 
    "This method is supposed to be local in ClassTrait because of a good reason.
    We use this pragma to test if ClassTrait does not contain some accidental 
    local selectors."

	[aTrait isBaseTrait] assert.
	baseTrait := aTrait
	
	