truncation and roundoff
expanded
	"Answer a Rectangle whose origin and corner are rounded to integers.
	Rounding is done by upper value on origin and lower value on corner so that
	self is inside rounded rectangle."

	^Rectangle origin: origin floor corner: corner ceiling