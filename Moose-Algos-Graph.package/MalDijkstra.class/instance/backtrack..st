backtracking
backtrack: b

	| p |
	b pathWeight = Float infinity
		ifTrue: [ ^ #() ]. "no solution"
	^ b = self start
		ifTrue: [ OrderedCollection with: b model ]
		ifFalse: [
			p := b previousNodes detectMin: [:n | n pathWeight].
			(self backtrack: p) add: b model; yourself ].