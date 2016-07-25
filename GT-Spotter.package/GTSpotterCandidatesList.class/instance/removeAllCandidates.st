adding/removing
removeAllCandidates

	self processors linksDo: [ :each | each clear ].
	self candidates removeAll.
	self notifyAllCandidatesRemoved