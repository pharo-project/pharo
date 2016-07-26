border-styles
drawWindowDiffuseDropShadowFor: aSystemWindow on: aCanvas
	"Draw the diffuse drop shadow for the given window."
	
	|or b r o gc|
	or := self windowActiveDropShadowOffsetRectangleFor: aSystemWindow.
	o := or bottom.
	(aSystemWindow roundedCorners includesAnyOf: #(2 3))
		ifTrue: [o := o + 2].
	b := aSystemWindow bounds expandBy: or.
	gc := {Color black alpha: 0.6. Color transparent}.
	r := b topLeft extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomRight;
					direction: o@0;
					radial: true).
	r := b topLeft + (o@0) extent: (b width - o - o @ o).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomLeft;
					direction: 0@o negated).
	r := b topRight - (o@0) extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomLeft;
					direction: o@0;
					radial: true).
	r := b topRight + (o negated@o) extent: (o @ (b height - o - o)).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: o@0).
	r := b bottomRight - o asPoint extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: o@0;
					radial: true).
	r := b bottomLeft + (o@o negated) extent: (b width - o - o @ o).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: 0@o).
	r := b bottomLeft - (0@o) extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topRight;
					direction: o@0;
					radial: true).
	r := b topLeft + (0@o) extent: (o @ (b height - o - o)).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topRight;
					direction: o negated@0)
