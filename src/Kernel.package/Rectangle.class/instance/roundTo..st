truncation and roundoff
roundTo: grid
	"Answer a Rectangle whose origin and corner are rounded to grid x and grid y."

	^Rectangle origin: (origin roundTo: grid)
				corner: (corner roundTo: grid)