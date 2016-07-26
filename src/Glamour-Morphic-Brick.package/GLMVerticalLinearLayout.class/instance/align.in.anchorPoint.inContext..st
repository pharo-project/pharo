layout
align: aBrick in: parentBrick anchorPoint: aPoint inContext: aContext
	|align|
	
	self = aContext ifTrue: [ ^ self ].
	(parentBrick subbricks size = 1) & (aContext isNil) ifTrue: [
		GLMHorizontalLinearLayout uniqueInstance align: aBrick in: parentBrick anchorPoint: aPoint inContext: self ].
	
	align := aBrick layout hAlign.
	
	align = #none ifTrue: [ ^ self ].
	align = #left ifTrue: [ ^ self ].
	align = #center ifTrue: [
		^ aBrick translateX: ((parentBrick innerBounds width - aBrick outerBounds width) / 2) asInteger ].
	align = #right ifTrue: [
		^ aBrick translateX: ((parentBrick innerBounds width - aBrick outerBounds width)) asInteger ].