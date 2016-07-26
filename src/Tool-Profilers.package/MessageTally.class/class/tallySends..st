public
tallySends: aBlock
	"
	MessageTally tallySends: [3.14159 printString]
	"
	
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."

	^ self tallySendsTo: nil inBlock: aBlock showTree: true