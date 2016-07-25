examples - branches
exampleIfFalseIfTrue3
	| a |
	a := true.
	a ifFalse: [ a := false  ] ifTrue: [ ^1 ].
	^ 2