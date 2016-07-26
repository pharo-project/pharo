brick-drawing
onDraw: aCanvas
	super onDraw: aCanvas.
	
		aCanvas
			fillRectangle: ((self brickBounds withWidth: self themer spotterThemer outerBorderWidth) translateBy: (self themer scrollbarWidth + self themer scrollbarOffset + 1)@0)
			color: (self spotterModel isPreviewVisible
				ifTrue: [ self themer spotterThemer backgroundColor ]
				ifFalse: [ Color transparent ])
	
	