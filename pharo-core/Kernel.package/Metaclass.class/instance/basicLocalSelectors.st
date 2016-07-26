accessing
basicLocalSelectors
	"Direct accessor for the instance variable localSelectors.
	Since localSelectors is lazily initialized, this may 
	return nil, which means that all selectors are local."

	^ localSelectors