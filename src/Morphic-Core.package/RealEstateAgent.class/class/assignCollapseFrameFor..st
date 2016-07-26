utilities
assignCollapseFrameFor: aSSView 
	"Offer up a location along the left edge of the screen for a collapsed
	SSView. Make sure it doesn't overlap any other collapsed frames."
	| grid otherFrames topLeft viewBox collapsedFrame extent newFrame verticalBorderDistance top |
	grid := 8.
	verticalBorderDistance := 8.
	otherFrames := ( aSSView world windowsSatisfying: [:w | w ~= aSSView])
				collect: [:w | w collapsedFrame]
				thenSelect: [:rect | rect notNil].
	viewBox := aSSView world viewBox.
	collapsedFrame := aSSView collapsedFrame.
	extent := collapsedFrame notNil
				ifTrue: [collapsedFrame extent]
				ifFalse: [aSSView getRawLabel width + aSSView labelWidgetAllowance @ (aSSView labelHeight + 2)].
	collapsedFrame notNil
		ifTrue: [(otherFrames
					anySatisfy: [:f | collapsedFrame intersects: f])
				ifFalse: ["non overlapping"
					^ collapsedFrame]].
	top := viewBox top + verticalBorderDistance.
	[topLeft := viewBox left @ top.
	newFrame := topLeft extent: extent.
	newFrame bottom <= (viewBox height - verticalBorderDistance)]
		whileTrue: [(otherFrames
					anySatisfy: [:w | newFrame intersects: w])
				ifFalse: ["no overlap"
					^ newFrame].
			top := top + grid].
	"If all else fails... (really to many wins here)"
	^ 0 @ 0 extent: extent