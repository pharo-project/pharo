drawing
drawBorderOn: aCanvas usingEnds: anArray 
	"Display my border on the canvas."
	"NOTE: Much of this code is also copied in  
	drawDashedBorderOn:  
	(should be factored)"
	| bigClipRect style |
	borderDashSpec
		ifNotNil: [^ self drawDashedBorderOn: aCanvas usingEnds: anArray].
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: self borderWidth + 1 // 2.
	self
		lineSegmentsDo: [:p1 :p2 | | p1i p2i | 
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			self hasArrows
				ifTrue: ["Shorten line ends so as not to interfere with tip  
					of arrow."
					((arrows == #back
								or: [arrows == #both])
							and: [p1 = vertices first])
						ifTrue: [p1i := anArray first asIntegerPoint].
					((arrows == #forward
								or: [arrows == #both])
							and: [p2 = vertices last])
						ifTrue: [p2i := anArray last asIntegerPoint]].
			(closed
					or: ["bigClipRect intersects: (p1i rect: p2i)  
						optimized:"
						((p1i min: p2i)
							max: bigClipRect origin)
							<= ((p1i max: p2i)
									min: bigClipRect corner)])
				ifTrue: [style
						drawLineFrom: p1i
						to: p2i
						on: aCanvas]]