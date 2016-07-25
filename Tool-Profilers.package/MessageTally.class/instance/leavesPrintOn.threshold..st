printing
leavesPrintOn: aStream threshold: threshold
	| dict |
	dict := IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	(dict asOrderedCollection
			select: [:node | node tally > threshold])
		asSortedCollection do: [:node |
			node printOn: aStream total: tally totalTime: time tallyExact: false ]