private
indexScore 

	| sum ramp |
	
	ramp := 1.
	sum := 0.
	
	1 to: indexes size - 1 do: [ :ix |
		ramp := (indexes at: ix) + 1 = (indexes at: ix + 1) 
			ifTrue: [ ramp + (ramp * self adjacencyIncrease) ]
			ifFalse: [ 1 ].		
		
		sum := sum + ramp - 1
	].
	
	^ sum rounded