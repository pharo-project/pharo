tests
testSize
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: dictionary size = 0.

	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self assert: dictionary size = i].

	self orderedAssociations size to: 1 by: -1 do: [:i |
		dictionary removeKey: (self orderedKeys at: i).
		self assert: dictionary size = (i - 1)].