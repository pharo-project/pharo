The same as a Set, except that items are compared using #== instead of #=.

Almost any class named IdentityFoo is the same as Foo except for the way items are compared.  In Foo, #= is used, while in IdentityFoo, #== is used.  That is, identity collections will treat items as the same only if they have the same identity.

For example, note that copies of a string are equal:

	('abc' copy) = ('abc' copy)

but they are not identitcal:

	('abc' copy) == ('abc' copy)

A regular Set will only include equal objects once:

	| aSet |
	aSet := Set new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet


An IdentitySet will include multiple equal objects if they are not identical:

	| aSet |
	aSet := IdentitySet new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet
