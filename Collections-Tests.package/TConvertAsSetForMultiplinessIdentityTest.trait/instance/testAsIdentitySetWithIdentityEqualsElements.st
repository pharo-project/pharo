tests - as identity set
testAsIdentitySetWithIdentityEqualsElements
	| result |
	result := self collectionWithIdentical asIdentitySet.
	" Only one element should have been removed as two elements are equals with Identity equality"
	self assert: result size = (self collectionWithIdentical size - 1).
	self collectionWithIdentical do: 
		[ :each | 
		(self collectionWithIdentical occurrencesOf: each) > 1 
			ifTrue: 
				[ "the two elements equals only with classic equality shouldn't 'have been removed"
				self assert: (result asOrderedCollection occurrencesOf: each) = 1
				" the other elements are still here" ]
			ifFalse: [ self assert: (result asOrderedCollection occurrencesOf: each) = 1 ] ].
	self assert: result class = IdentitySet