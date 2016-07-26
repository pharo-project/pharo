drawing
onDrawAthensCanvas: aCanvas
	|wrappedCanvas|
	
	super onDrawAthensCanvas: aCanvas.
	wrappedCanvas := AthensCanvasWrapper on: aCanvas.
	
	self highlightedText isEmpty ifFalse: [
		self calculateHighlightingBounds do: [ :each |
			self highlightingLogic
				ifNil: [ wrappedCanvas line: each bottomLeft to: each bottomRight width: 1 color: self textColor ]
				ifNotNil: [ self highlightingLogic cull: wrappedCanvas cull: each ] ] ].
		
	aCanvas pathTransform restoreAfter:[ 
			aCanvas pathTransform translateBy: (self calculateTextPosition - self brickBounds origin).
			aCanvas pathTransform translateX: 0 Y: font getPreciseAscent.
			self flag: #todo.	"scaled, so that FT-Plugin and Athens don't mix cached faces"
			aCanvas pathTransform scaleBy: 1.0000001 @ 1.0000001.
			aCanvas setPaint: self textColor.
			aCanvas setFont: self font.
			aCanvas drawString: self text from: 1 to: self text size ].
		
	wrappedCanvas
		drawString: self text
		at: (self calculateTextPosition - self brickBounds origin)
		font: self font
		color: self textColor