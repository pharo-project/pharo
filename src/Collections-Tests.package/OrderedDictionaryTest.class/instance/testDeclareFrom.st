tests
testDeclareFrom
	| dictionary otherDictionary |

	dictionary := self emptyDictionary.
	otherDictionary := self dictionaryWithAssociations.
	self dictionaryKeys do: [:each |
		self assert:
			(dictionary
				declare: each
				from: otherDictionary) == dictionary].
	self
		assertIsDictionary: dictionary
		withAssociations: self dictionaryAssociations.
	self assert: otherDictionary isEmpty.

	self dictionaryKeys do: [:each |
		otherDictionary add: (each -> self newValue).
		self assert:
			(dictionary
				declare: each
				from: otherDictionary) == dictionary].
	self
		assertIsDictionary: dictionary
		withAssociations: self dictionaryAssociations.