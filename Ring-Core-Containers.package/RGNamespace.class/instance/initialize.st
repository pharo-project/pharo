initialization
initialize

	super initialize.
	self globalVariables: IdentityDictionary new.
	self pools: IdentityDictionary new.