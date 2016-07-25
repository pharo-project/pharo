tests
testSize
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: dictionary size isZero.

	self dictionaryAssociations doWithIndex: [:each :i |
		dictionary add: each.
		self assert: dictionary size = i].

	self dictionaryAssociations size to: 1 by: -1 do: [:i |
		dictionary removeKey: (self dictionaryKeys at: i).
		self assert: dictionary size = (i - 1)].