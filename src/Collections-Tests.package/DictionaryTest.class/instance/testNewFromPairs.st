tests - new
testNewFromPairs
	| assocs pairs |

	assocs := self nonEmptyDict associations.
	pairs := OrderedCollection new.
	assocs do: [:each |
		pairs
			add: each key;
			add: each value].
	0 to: pairs size do: [:i | | dictFromPairs dictFromAssocs |
		dictFromPairs := self classToBeTested newFromPairs: (pairs copyFrom: 1 to: i).
		dictFromAssocs := self classToBeTested newFrom: (assocs copyFrom: 1 to: (i / 2) floor).
		self assert: dictFromPairs = dictFromAssocs].