drawing
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"Draw a beveled or raised rectangle"
	| bw |

	"Note: The optimization test below should actually read:
		self ifNoTransformWithIn: (r insetBy: borderWidth // 2)
	but since borderWidth is assumed to be very small related to r we don't check it."

	(self ifNoTransformWithIn: r)
		ifTrue:[^super frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor].

	"Fill rectangle and draw top and left border"
	bw := borderWidth // 2.
	self drawRectangle: (r insetBy: bw)
		color: fillColor
		borderWidth: borderWidth
		borderColor: topLeftColor.
	"Now draw bottom right border."
	self drawPolygon: (Array with: r topRight + (bw negated@bw) 
							with: r bottomRight - bw asPoint
							with: r bottomLeft + (bw@bw negated))
		color: nil
		borderWidth: borderWidth
		borderColor: bottomRightColor.