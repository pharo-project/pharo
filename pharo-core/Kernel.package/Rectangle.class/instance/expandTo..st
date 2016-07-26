truncation and roundoff
expandTo: grid
	"Answer a Rectangle whose origin and corner are rounded to grid x and grid y.
	Rounding is done by upper value on origin and lower value on corner so that
	self is inside rounded rectangle."

	^Rectangle origin: (origin roundDownTo: grid)
				corner: (corner roundUpTo: grid)