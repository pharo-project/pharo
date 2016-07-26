private-compute-index
createEmptyRangeForGapAt: index 
	| current previous |
	current := ranges at: index.
	previous := ranges at: (index - 1 max: 1).
	self 
		insertEmptyRangeAt: index
		start: previous end + 1
		end: current start - 1.
	^ index