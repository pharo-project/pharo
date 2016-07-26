tests
testEquals
	| dictionaryOne dictionaryTwo |

	dictionaryOne := self emptyDictionary.
	dictionaryTwo := self emptyDictionary.
	self assertDictionary: dictionaryOne equals: dictionaryTwo.

	"For equality, order will not matter"
	self dictionaryAssociations
		with: self dictionaryAssociations reversed
		do: [:firstAssociation :secondAssociation |
			dictionaryOne add: firstAssociation.
			dictionaryTwo add: secondAssociation.
			self
				assertDictionary: dictionaryOne doesNotEqual: self emptyDictionary;
				assertDictionary: dictionaryTwo doesNotEqual: self emptyDictionary.
			dictionaryOne size < self dictionaryAssociations size
				ifTrue: [self assertDictionary: dictionaryOne doesNotEqual: dictionaryTwo]].

	self assertDictionary: dictionaryOne equals: dictionaryTwo.