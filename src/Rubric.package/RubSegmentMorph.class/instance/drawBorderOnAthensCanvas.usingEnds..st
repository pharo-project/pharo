drawing
drawBorderOnAthensCanvas: aCanvas usingEnds: anArray
	"Display my border on the canvas."

	| bigClipRect style |
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: (self borderWidth + 1) // 2.
	self
		lineSegmentsDo: [ :p1 :p2 | 
			| p1i p2i path |
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			path := aCanvas
				createPath: [ :builder | 
					builder absolute.
					builder moveTo: p1i.
					builder lineTo: p2i ].
			(aCanvas setStrokePaint: style color) width: style width.
			aCanvas drawShape: path ]