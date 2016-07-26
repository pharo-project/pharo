examples
exampleIfTrueIfFalse
	| t |
	t := false.
	t ifTrue: [ self bar ] ifFalse: [ 'hello' ].
	^ 5 