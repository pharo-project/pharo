as yet unclassified
rowAtLocation: aPoint
	"return the number of the row at aPoint"

	| y |
	y := aPoint y.
	y < self top ifTrue: [ ^ 1 ].
	listItems with: (1 to: listItems size) do: [:i :row |
		(y < i bottom) ifTrue: [^row]].
	^listItems size