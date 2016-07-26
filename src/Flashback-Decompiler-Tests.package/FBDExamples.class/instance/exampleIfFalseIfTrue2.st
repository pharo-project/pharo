examples - branches
exampleIfFalseIfTrue2
	| a |
	a := true.
	a ifFalse: [ a := 1 + 2 ] ifTrue: [ a := 2 + 3 ]