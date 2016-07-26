traits
ensureLocalSelectors
	"Ensures that the instance variable localSelectors is effectively used to maintain
	the set of local selectors.
	This method must be called before any non-local selectors are added to the
	method dictionary!"

	self basicLocalSelectors ifNil: [self basicLocalSelectors: self selectors asSet]