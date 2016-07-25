testing
testIsLocalAliasSelector
	self deny: (self t1 isLocalAliasSelector: #m11).
	self deny: (self t1 isLocalAliasSelector: #foo).

	"directly"
	self assert: (self t6 isLocalAliasSelector: #m22Alias).
	self deny: (self t6 isLocalAliasSelector: #m22).

	"indirectly"
	self c1 setTraitComposition: self t6 asTraitComposition.
	self deny: (self c1 isLocalAliasSelector: #m22Alias).
	self deny: (self c1 isLocalAliasSelector: #m22)