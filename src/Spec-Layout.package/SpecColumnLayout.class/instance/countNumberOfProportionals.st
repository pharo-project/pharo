private
countNumberOfProportionals

	self commands	do: [ :e | 
		e layoutFrame hasNoOffsets
			ifTrue: [ 
				(e layoutFrame topFraction == 0 and: [ e layoutFrame bottomFraction == 1 ])
					ifFalse: [ autoFractionComputation := false ].
				numberOfProportionals := numberOfProportionals + 1 ] ]