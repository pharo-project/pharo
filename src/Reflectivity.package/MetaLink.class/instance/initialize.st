initialization
initialize
	nodes := IdentitySet new.
	control := #before.
	selector := #value.
	arguments := #().
	options := Set new.
	self parseOptions: self class defaultOptions.