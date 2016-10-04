tests
testEquals
	| dictionaryOne dictionaryTwo |

	dictionaryOne := self emptyDictionary.
	dictionaryTwo := self emptyDictionary.
	self
		assertDictionary: dictionaryOne
		equals: dictionaryTwo.

	"For equality, order will not matter"
	self orderedAssociations
		with: self orderedAssociations reversed
		do: [:firstAssociation :secondAssociation |
			dictionaryOne add: firstAssociation.
			dictionaryTwo add: secondAssociation.
			self
				assertDictionary: dictionaryOne
				doesNotEqual: self emptyDictionary.
			self
				assertDictionary: dictionaryTwo
				doesNotEqual: self emptyDictionary.
			dictionaryOne size < self orderedAssociations size
				ifTrue: [
					self
						assertDictionary: dictionaryOne
						doesNotEqual: dictionaryTwo]].

	self
		assertDictionary: dictionaryOne
		equals: dictionaryTwo.