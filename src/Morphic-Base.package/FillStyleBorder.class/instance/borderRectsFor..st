geometry
borderRectsFor: aRectangle
	"Answer a collection of rectangles to fill.
	Just four here for a rectangular border."

	|rTop rBottom rLeft rRight w|
	w := self width.
	rTop := aRectangle topLeft corner: aRectangle right @ (aRectangle top + w).
	rBottom := aRectangle left @ (aRectangle bottom - w) corner: aRectangle bottomRight.
	rLeft := aRectangle left @ (aRectangle top + w) corner: aRectangle left + w @ (aRectangle bottom - w).
	rRight := aRectangle right - w @ (aRectangle top + w) corner: aRectangle right @ (aRectangle bottom - w).
	^{rTop. rBottom. rLeft. rRight}