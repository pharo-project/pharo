testing
testLongLine
	| input output |
	
	input := (String new: 74 withAll: $A), (String new: 47 withAll: $B).
	
	output := (String new: 74 withAll: $A), '=
', (String new: 47 withAll: $B).
	
	self 
		assert: (self encode: input)
		equals: output