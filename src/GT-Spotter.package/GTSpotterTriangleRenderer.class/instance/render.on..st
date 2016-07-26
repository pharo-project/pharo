rendering
render: aBrick on: aCanvas
	
	self orientation = #right ifTrue: [ self renderRightTriangle: aBrick on: aCanvas ].
	self orientation = #bottom ifTrue: [ self renderBottomTriangle: aBrick on: aCanvas ].
	self orientation = #left ifTrue: [ self renderLeftTriangle: aBrick on: aCanvas ].
	self orientation = #top ifTrue: [ self renderTopTriangle: aBrick on: aCanvas ].