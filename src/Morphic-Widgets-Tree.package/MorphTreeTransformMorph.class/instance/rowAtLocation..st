drawing
rowAtLocation: aPoint
	"return the number of the row at aPoint"
	| y |
	y := aPoint y.
	submorphs ifEmpty: [^ nil].
	submorphs doWithIndex:  [:m :idx | m topLeft y >= y ifTrue: [^ (idx - 1) max: 1]].
	^ submorphs size