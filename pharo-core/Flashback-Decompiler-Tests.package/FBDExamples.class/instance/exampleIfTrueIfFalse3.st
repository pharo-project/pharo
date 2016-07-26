examples - branches
exampleIfTrueIfFalse3
	| a |
	a := true.
	a ifTrue: [ ^ 1 ] ifFalse: [ a := false ].
	^ 2