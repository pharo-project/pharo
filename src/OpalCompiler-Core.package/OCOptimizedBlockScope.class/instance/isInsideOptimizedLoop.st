testing
isInsideOptimizedLoop
	^ isInlinedLoop
		ifTrue: [true]
		ifFalse: [self outerScope isInsideOptimizedLoop]