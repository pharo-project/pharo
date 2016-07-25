private
countNumberOfProportionals

	self commands do: [ :e | 
		e layoutFrame hasNoOffsets
			ifTrue: [ 
				(e layoutFrame leftFraction == 0 and: [ e layoutFrame rightFraction == 1 ])
					ifFalse: [ autoFractionComputation := false ].
				numberOfProportionals := numberOfProportionals + 1 ] ]