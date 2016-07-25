rectangle functions
bordersOn: her along: herSide 
	((herSide = #right and: [self left = her right])
	or: [herSide = #left and: [self right = her left]])
		ifTrue:
		[^ (self top max: her top) < (self bottom min: her bottom)].
	((herSide = #bottom and: [self top = her bottom])
	or: [herSide = #top and: [self bottom = her top]])
		ifTrue:
		[^ (self left max: her left) < (self right min: her right)].
	^ false