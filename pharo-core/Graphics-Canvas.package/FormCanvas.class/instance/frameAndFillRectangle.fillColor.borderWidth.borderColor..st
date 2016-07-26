drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	rect := r translateBy: origin.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setFillColor: borderColor.
		(r area > 10000 or: [fillColor isTranslucent]) ifTrue: [
			port frameRect: rect borderWidth: borderWidth.
		] ifFalse: ["for small rectangles, it's faster to fill the entire outer rectangle
					than to compute and fill the border rects"
					port fillRect: rect offset: origin]].

	"fill the inside"
	fillColor isTransparent ifFalse:
		[self setFillColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) offset: origin].