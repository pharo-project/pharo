tests
testIt: anArgument 
	| loc1 x2 t |
	x2 := [:bar :var | var < bar].
	loc1 := constantArray.
	x2 value: loc1.
	t := 15.
	^ x2