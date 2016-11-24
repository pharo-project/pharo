accessing
basicLocalSelectors: aSetOrNil

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	localSelectors := aSetOrNil