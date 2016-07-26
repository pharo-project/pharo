tests - adding
testAddDuplicateItem1
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection add: 'John' before: 'John'.
	self
		assert: ((collection asBag occurrencesOf: 'John')
					= 2
				and: [(collection at: (collection indexOf: 'John')
							+ 1)
						= (collection
								at: (collection indexOf: 'John'))])