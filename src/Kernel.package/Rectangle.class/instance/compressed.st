truncation and roundoff
compressed
	"Answer a Rectangle whose origin and corner are rounded to integers.
	Rounding is done by upper value on origin and lower value on corner so that
	rounded rectangle is inside self."

	^Rectangle origin: origin ceiling corner: corner floor