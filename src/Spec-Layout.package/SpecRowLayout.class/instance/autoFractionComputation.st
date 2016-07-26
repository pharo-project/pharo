private
autoFractionComputation
	
	self commands
		doWithIndex: [ :e :idx | 
			e layoutFrame hasNoOffsets
				ifTrue: [ | delta |
					delta := e layoutFrame rightFraction - e layoutFrame leftFraction.
					self setWidgetLayoutWithNoOffset: e at: idx delta: delta ]
				ifFalse: [ self setWidgetLayoutWithOffset: e at: idx ] ]