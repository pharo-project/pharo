accessing
basicLocalSelectors
	"Direct accessor for the instance variable localSelectors.
	Since localSelectors is lazily initialized, this may 
	return nil, which means that all selectors are local."

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ localSelectors