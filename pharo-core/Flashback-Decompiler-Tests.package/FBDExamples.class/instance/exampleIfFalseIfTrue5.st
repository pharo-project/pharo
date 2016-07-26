examples - branches
exampleIfFalseIfTrue5
	| a |
	a := true.
	a ifFalse: [ ^1 ] ifTrue: [ 1 + 2 ]