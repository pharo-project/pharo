testing
testIsAliasSelector
	self deny: (self t1 isAliasSelector: #m11).
	self deny: (self t1 isAliasSelector: #foo).

	"directly"
	self assert: (self t6 isAliasSelector: #m22Alias).
	self deny: (self t6 isAliasSelector: #m22).

	"indirectly"
	self c1 setTraitCompositionFrom: self t6.
	self assert: (self c1 isAliasSelector: #m22Alias).
	self deny: (self c1 isAliasSelector: #m22)