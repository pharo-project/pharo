drawing-rectangles
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"Draw the rectangle using the given attributes.
	Note: This is a *very* simple implementation"
	| bw pt |
	self frameAndFillRectangle: r
		fillColor: fillColor
		borderWidth: borderWidth
		borderColor: bottomRightColor.
	bottomRightColor = topLeftColor ifFalse: [
		bw := borderWidth asPoint.
		pt := r topLeft + (bw // 2).
		self line: pt to: pt + ((r extent x - bw x)@0) width: borderWidth color: topLeftColor.
		self line: pt to: pt + (0@(r extent y - bw y)) width: borderWidth color: topLeftColor.
	].