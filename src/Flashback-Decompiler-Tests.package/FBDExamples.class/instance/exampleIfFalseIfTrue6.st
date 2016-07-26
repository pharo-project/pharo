examples - branches
exampleIfFalseIfTrue6
	| a |
	a := true.
	^ a ifFalse: [ ] ifTrue: [ ^1 ].
	