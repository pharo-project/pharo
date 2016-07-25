tests - including with identity
testIdentityIncludes
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "
	" can not be used by collections that can't include elements for wich copy doesn't return another instance "
	| collection anElement |
	
	collection := self collectionWithCopyNonIdentical.
	anElement := collection anyOne copy.

	self deny: (collection identityIncludes: anElement)