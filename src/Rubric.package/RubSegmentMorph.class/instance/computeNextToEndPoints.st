smoothing
computeNextToEndPoints
	| pointAfterFirst pointBeforeLast |
	pointAfterFirst := nil.
	self lineSegmentsDo: 
			[:p1 :p2 | 
			pointAfterFirst ifNil: [pointAfterFirst := p2 asIntegerPoint].
			pointBeforeLast := p1 asIntegerPoint].
	curveState at: 2 put: pointAfterFirst.
	curveState at: 3 put: pointBeforeLast