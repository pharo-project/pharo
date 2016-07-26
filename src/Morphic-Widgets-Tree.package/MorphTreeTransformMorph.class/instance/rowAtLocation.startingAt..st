drawing
rowAtLocation: aPoint startingAt: aPosition
	"return the number of the row at aPoint"
	| y |
	y := aPoint y.
	submorphs ifEmpty: [^ nil].
	aPosition to: submorphs size do: [ :idx | | m |
		m := submorphs basicAt: idx.
		m topLeft y >= y ifTrue: [^ (idx - 1) max: 1]].
	^ submorphs size