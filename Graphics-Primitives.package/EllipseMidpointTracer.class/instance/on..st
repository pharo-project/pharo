initialize
on: aRectangle
	a := aRectangle width // 2.
	b := aRectangle height // 2.
	x := 0.
	y := b.
	aSquared := a * a.
	bSquared := b * b.
	d1 := bSquared - (aSquared * b) + (0.25 * aSquared).
	d2 := nil.
	inFirstRegion := true.