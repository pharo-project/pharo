accessing parallel hierarchy
classTrait: aTrait
	"Assigns the class trait associated with the receiver."
	
	<localTraitMethod> 
    "This method is supposed to be local in Trait because of a good reason.
    We use this pragma to test if Trait does not contain some accidental 
    local selectors."
	
	[aTrait isClassTrait] assert.
	classTrait := aTrait