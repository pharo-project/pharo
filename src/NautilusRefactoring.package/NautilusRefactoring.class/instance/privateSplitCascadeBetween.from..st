private-source
privateSplitCascadeBetween: anInterval from: aMethod
	^ RBSplitCascadeRefactoring
		model: environment
		split: anInterval
		from: aMethod selector
		in: aMethod methodClass