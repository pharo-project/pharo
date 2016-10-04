tests
testAdd
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self
			deny: (dictionary includesAssociation: each);
			assert: (dictionary add: each) == each;
			assert: (dictionary includesAssociation: each).
		self
			assertIsDictionary: dictionary
			withOrderedAssociations: (self orderedAssociationsFirst: i)].

	"ensure adding the same associations doesn't change the order"
	self orderedAssociations reverseDo: [:each |
		self assert: (dictionary add: each) == each.
		self
			assertIsDictionary: dictionary
			withOrderedAssociations: self orderedAssociations].

	self changedOrderedAssociations withIndexDo: [:each :i | | old |
		old := self orderedAssociations at: i.
		self
			assert: (dictionary add: each) == each;
			assert: (dictionary includesAssociation: each);
			deny: (dictionary includesAssociation: old).
		self
			assertIsDictionary: dictionary
			withOrderedAssociations:
				(self changedOrderedAssociationsFirst: i),
				(self orderedAssociationsAllButFirst: i)].