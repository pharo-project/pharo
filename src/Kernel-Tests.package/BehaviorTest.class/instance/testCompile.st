tests
testCompile
	| behavior |
	behavior := Behavior new.
	behavior compile: 'thisIsATest ^2'.
	behavior new thisIsATest