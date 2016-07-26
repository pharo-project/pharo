initialization
initialize

	super initialize.
	self definedClasses: IdentityDictionary new.
	self methods: IdentityDictionary new.
	self packages: IdentityDictionary new.