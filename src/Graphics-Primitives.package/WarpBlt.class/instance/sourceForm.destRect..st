primitives
sourceForm: srcForm destRect: dstRectangle 
	"Set up a WarpBlt from the entire source Form to the given destination rectangle."
	| w h |
	sourceForm := srcForm.
	sourceX := sourceY := 0.
	destX := dstRectangle left.
	destY := dstRectangle top.
	width := dstRectangle width.
	height := dstRectangle height.
	w := 16384 * (srcForm width - 1).
	h := 16384 * (srcForm height - 1).
	p1x := 0.
	p2x := 0.
	p3x := w.
	p4x := w.
	p1y := 0.
	p2y := h.
	p3y := h.
	p4y := 0.
	p1z := p2z := p3z := p4z := 16384	"z-warp ignored for now"