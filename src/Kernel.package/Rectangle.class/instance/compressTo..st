truncation and roundoff
compressTo: grid
	"Answer a Rectangle whose origin and corner are rounded to grid x and grid y.
	Rounding is done by upper value on origin and lower value on corner so that
	rounded rectangle is inside self."

	^Rectangle origin: (origin roundUpTo: grid)
				corner: (corner roundDownTo: grid)