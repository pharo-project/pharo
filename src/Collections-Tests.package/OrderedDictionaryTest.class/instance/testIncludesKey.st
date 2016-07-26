tests
testIncludesKey
	| dictionary associations |

	dictionary := self emptyDictionary.
	self deny: (self dictionaryKeys anySatisfy: [:each | dictionary includesKey: each]).

	associations := self dictionaryAssociations asOrderedCollection.
	[associations notEmpty]
		whileTrue: [| addedAssociation |
			addedAssociation := dictionary add: associations removeFirst.
			self assert: (dictionary includesKey: addedAssociation key).

			associations do: [:each |
				self deny: (dictionary includesKey: each key)]].