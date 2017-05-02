accessing parallel hierarchy
classTrait: aClassTrait

	<localClassTraitMethod> 
    "This method is supposed to be local in ClassTrait because of a good reason.
    We use this pragma to test if ClassTrait does not contain some accidental 
    local selectors."

	self error: 'Trait is already a class trait!'
	
	