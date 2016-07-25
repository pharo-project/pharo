tests
testIntervals
	| intervals |
	intervals := STON listClass withAll: { 
		1 to: 10.
		1 to: 10 by: 2.
		100 to: 50 by: -5 }.
	self serializeAndMaterialize: intervals