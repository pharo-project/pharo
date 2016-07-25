backtracking
backtrack: b edgeDo: aBlock
	"special purpose backtracking for CSE: backtrack through edge and do something on each edge 
	while traversing.
	 do not check if there is a solution (pathWeight ~= Float infinity)"

	| pe p |
	^ b = self start
		ifTrue: [ OrderedCollection with: b model ]
		ifFalse: [
			pe := b previousEdges detectMin: [:e | e from pathWeight].
			p := pe from.
			aBlock value: pe.
			(self backtrack: p edgeDo: aBlock) add: b model; yourself ].