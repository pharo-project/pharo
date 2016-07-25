cubic support
honeIn: centerN step: step measure: measure withIn: closeEnough 
	"Pick the best n by binary search."
	
	| nTry |
	step < 1
		ifTrue: [^ centerN].
	nTry := centerN - step.
	^ measure > (closeEnough + (self measureFor: nTry))
		ifTrue: [self
				honeIn: centerN
				step: step // 2
				measure: measure
				withIn: closeEnough]
		ifFalse: [self
				honeIn: nTry
				step: step // 2
				measure: measure
				withIn: closeEnough]