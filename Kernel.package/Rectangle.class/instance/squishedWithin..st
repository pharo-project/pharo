transforming
squishedWithin: aRectangle
	"Return an adjustment of the receiver that fits within aRectangle by reducing its size, not by changing its origin.  "

	^ origin corner: (corner min: aRectangle bottomRight)

"(50 @ 50 corner: 160 @ 100) squishedWithin:  (20 @ 10 corner: 90 @ 85)"
