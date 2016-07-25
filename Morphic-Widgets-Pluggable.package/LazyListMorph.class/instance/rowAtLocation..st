list management
rowAtLocation: aPoint
	"return the number of the row at aPoint"
	| y |
	y := aPoint y.
	y < self top ifTrue: [ ^ 1 ].
	^((y - self top // (font height)) + 1) min: listItems size max: 0