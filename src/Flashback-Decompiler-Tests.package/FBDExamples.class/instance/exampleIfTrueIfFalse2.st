examples - branches
exampleIfTrueIfFalse2
	| a |
	a := true.
	a ifTrue: [ a:= 1 + 2 ] ifFalse: [ a := 2 + 3 ]