tests
testAddAll
	| dictionary addedAssociations |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includesAssociation: each)].

	addedAssociations := self orderedAssociations.
	self assert: (dictionary addAll: addedAssociations) == addedAssociations.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.

	"ensure adding the same associations doesn't change the order"
	addedAssociations := self orderedAssociations reversed.
	self assert: (dictionary addAll: addedAssociations) == addedAssociations.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.

	addedAssociations := self changedOrderedAssociations.
	self assert: (dictionary addAll: addedAssociations) == addedAssociations.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self changedOrderedAssociations.