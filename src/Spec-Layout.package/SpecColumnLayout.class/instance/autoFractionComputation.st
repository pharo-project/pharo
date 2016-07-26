private
autoFractionComputation
	| delta |
	
	numberOfProportionals isZero
		ifTrue: [ delta := 0 ]
		ifFalse: [ delta := 1 / numberOfProportionals ].
	
	self commands doWithIndex: [ :e :idx | 
		e layoutFrame hasNoOffsets
			ifTrue: [ self setWidgetLayoutWithNoOffset: e at: idx delta: delta ]
			ifFalse: [ self setWidgetLayoutWithOffset: e at: idx ] ]