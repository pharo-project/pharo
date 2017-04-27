selecting
selectFirst
	"Select the first candidate.
	Note: there must be at least one candidate in order to select first,
	make sure to check #hasCandidates before using me"
	self
		assert: [ self candidates isNotEmpty ]
		description: [ 'There must be at least one candidate in order to select the first' ].

	self select: self candidates head