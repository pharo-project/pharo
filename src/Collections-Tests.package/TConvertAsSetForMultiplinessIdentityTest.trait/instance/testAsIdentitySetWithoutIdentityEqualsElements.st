tests - as identity set
testAsIdentitySetWithoutIdentityEqualsElements
	| result collection |
	collection := self collectionWithCopy.
	result := collection asIdentitySet.
	" no elements should have been removed as no elements are equels with Identity equality"
	self assert: result size = collection size.
	collection do: 
		[ :each | 
		(collection occurrencesOf: each) = (result asOrderedCollection occurrencesOf: each) ].
	self assert: result class = IdentitySet