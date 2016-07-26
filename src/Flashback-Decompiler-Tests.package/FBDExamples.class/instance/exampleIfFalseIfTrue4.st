examples - branches
exampleIfFalseIfTrue4
	| a |
	a := true.
	a ifFalse: [ 1 + 2 ] ifTrue: [ ^1 ]