initialization
initialize
	super initialize.
	self patterns do: [ :halt | self addRuleRemoving: halt ]