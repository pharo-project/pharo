arithmetic
= comparand
	^ self species = comparand species 
		and: [ self start = comparand start 
				and: [ self duration = comparand duration ]]
