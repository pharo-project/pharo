private
manualFractionComputation
	
	self commands	doWithIndex: [ :e :idx | 
		e layoutFrame hasNoOffsets
			ifTrue: [  | delta |
				delta := e layoutFrame bottomFraction - e layoutFrame topFraction.
				self setWidgetLayoutWithNoOffset: e at: idx delta: delta ]
			ifFalse: [ self setWidgetLayoutWithOffset: e at: idx ] ]