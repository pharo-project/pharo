drawing
drawLineFrom: startPoint to: stopPoint on: aCanvas 
	"Here we're using the balloon engine since this is much faster than BitBlt w/ brushes."

	| delta length dir cos sin tfm w h w1 w2 h1 h2 fill |
	width isPoint 
		ifTrue: 
			[w := width x.
			h := width y]
		ifFalse: [w := h := width].
	w1 := w // 2.
	w2 := w - w1.
	h1 := h // 2.
	h2 := h - h1.
	"Compute the rotational transform from (0@0) -> (1@0) to startPoint -> stopPoint"
	delta := stopPoint - startPoint.
	length := delta r.
	dir := length > 1.0e-10 ifTrue: [delta / length] ifFalse: [ 1 @ 0].
	cos := dir dotProduct: 1 @ 0.
	sin := dir crossProduct: 1 @ 0.
	tfm := (MatrixTransform2x3 new)
				a11: cos;
				a12: sin;
				a21: sin negated;
				a22: cos.
	"Install the start point offset"
	tfm offset: startPoint.
	"Now get the fill style appropriate for the given direction"
	fill := self fillStyleForDirection: dir.
	"And draw..."
	aCanvas asBalloonCanvas transformBy: tfm
		during: 
			[:cc | 
			cc drawPolygon: { 
						(0 - w1) @ (0 - h1).	"top left"
						(length + w2) @ (0 - h1).	"top right"
						(length + w2) @ h2.	"bottom right"
						(0 - w1) @ h2	"bottom left"}
				fillStyle: fill]