accessing-computed
leaveItems
	| res  |
	res := OrderedCollection new.
	rootMessageTally
		ifNotNil: [ 
			| dict |
			dict := IdentityDictionary new: 100.
			rootMessageTally leavesInto: dict fromSender: nil.
			res addAll: (dict asOrderedCollection select: [ :node | node tally > self threshold ]) ].
	^ res