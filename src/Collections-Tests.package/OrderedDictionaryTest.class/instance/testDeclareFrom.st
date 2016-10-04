tests
testDeclareFrom
	| dictionary otherDictionary |

	dictionary := self emptyDictionary.
	otherDictionary := self dictionaryWithOrderedAssociations.
	self orderedKeys do: [:each |
		self assert:
			(dictionary
				declare: each
				from: otherDictionary) == dictionary].
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self assert: otherDictionary isEmpty.

	self orderedKeys do: [:each |
		otherDictionary add: (each -> self newValue).
		self assert:
			(dictionary
				declare: each
				from: otherDictionary) == dictionary].
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.