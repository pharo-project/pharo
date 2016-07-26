layout
align: aBrick in: parentBrick anchorPoint: aPoint inContext: aContext
	|align|
	
	self = aContext ifTrue: [ ^ self ].
	(parentBrick subbricks size = 1) & (aContext isNil) ifTrue: [
		GLMVerticalLinearLayout uniqueInstance align: aBrick in: parentBrick anchorPoint: aPoint inContext: self ].
	
	align := aBrick layout vAlign.
	
	align = #none ifTrue: [ ^ self ].
	align = #center ifTrue: [
		^ aBrick translateY: ((parentBrick innerBounds height - aBrick outerBounds height) / 2) asInteger ].
	align = #bottom ifTrue: [
		^ aBrick translateY: ((parentBrick innerBounds height - aBrick outerBounds height)) asInteger ].