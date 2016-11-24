accessing
traitComposition

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	traitComposition ifNil: [traitComposition := TraitComposition new].
	^traitComposition