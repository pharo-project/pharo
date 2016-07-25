private-compute-index
computeIndexOfPosition
	| current |
	self checkForZeroPosition ifTrue: [ ^ 1 ].
	1 
		to: ranges size
		do: 
			[ :index | 
			current := ranges at: index.
			(current includesPosition: position) 
				ifTrue: [ ^ index ]
				ifFalse: 
					[ current end > position ifTrue: [ ^ self createEmptyRangeForGapAt: index ] ] ].
	^ self createEmptyRangeAtTail