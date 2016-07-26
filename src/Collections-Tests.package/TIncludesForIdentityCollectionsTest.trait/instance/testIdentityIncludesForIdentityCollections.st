tests - including for identity collections
testIdentityIncludesForIdentityCollections
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "
	" can not be used by collections that can't include elements for wich copy doesn't return another instance "
	| collection element |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	element := collection anyOne copy.
	self deny: (collection identityIncludes: element)