drawing
drawDashedBorderOn: aCanvas usingEnds: anArray 
	"Display my border on the canvas. NOTE: mostly copied from  
	drawBorderOn:"
	| bevel topLeftColor bottomRightColor bigClipRect lineColor segmentOffset |
	(borderColor isNil
			or: [borderColor isColor
					and: [borderColor isTransparent]])
		ifTrue: [^ self].
	lineColor := borderColor.
	bevel := false.
	"Border colors for bevelled effects depend on CW ordering of  
	vertices"
	borderColor == #raised
		ifTrue: [topLeftColor := color lighter.
			bottomRightColor := color darker.
			bevel := true].
	borderColor == #inset
		ifTrue: [topLeftColor := owner colorForInsets darker.
			bottomRightColor := owner colorForInsets lighter.
			bevel := true].
	bigClipRect := aCanvas clipRect expandBy: self borderWidth + 1 // 2.
	segmentOffset := self borderDashOffset.
	self
		lineSegmentsDo: [:p1 :p2 | | p2i p1i | 
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
				ifTrue: [bevel
						ifTrue: [lineColor := (p1i quadrantOf: p2i)
											> 2
										ifTrue: [topLeftColor]
										ifFalse: [bottomRightColor]].
					segmentOffset := aCanvas
								line: p1i
								to: p2i
								width: borderWidth
								color: lineColor
								dashLength: borderDashSpec first
								secondColor: borderDashSpec third
								secondDashLength: borderDashSpec second
								startingOffset: segmentOffset]]