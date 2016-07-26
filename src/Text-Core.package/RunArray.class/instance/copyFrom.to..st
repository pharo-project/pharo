copying
copyFrom: start to: stop
	| newRuns run1 run2 offset1 offset2 | 
	stop < start ifTrue: [^RunArray new].
	self at: start setRunOffsetAndValue: [:r :o :value1 | run1 := r. offset1 := o. value1].
	self at: stop setRunOffsetAndValue: [:r :o :value2 | run2 := r. offset2 := o. value2].
	run1 = run2
		ifTrue: 
			[newRuns := Array with: offset2 - offset1 + 1]
		ifFalse: 
			[newRuns := runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1].
	^RunArray runs: newRuns values: (values copyFrom: run1 to: run2)