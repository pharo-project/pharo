drawing
drawBorderOn: aCanvas usingEnds: anArray
	"Display my border on the canvas."

	| bigClipRect style |
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: (self borderWidth + 1) // 2.
	self
		lineSegmentsDo: [ :p1 :p2 | 
			| p1i p2i |
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			style drawLineFrom: p1i to: p2i on: aCanvas ]