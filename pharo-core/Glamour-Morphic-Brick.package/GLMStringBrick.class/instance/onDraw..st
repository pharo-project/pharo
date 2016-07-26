drawing
onDraw: aCanvas

	super onDraw: aCanvas.
	
	self highlightedText isEmpty ifFalse: [
		self calculateHighlightingBounds do: [ :each |
			self highlightingLogic
				ifNil: [ aCanvas line: each bottomLeft to: each bottomRight width: 1 color: self textColor ]
				ifNotNil: [ self highlightingLogic cull: aCanvas cull: each ] ] ].
	
	aCanvas
		drawString: self text
		at: self calculateTextPosition
		font: self font
		color: self textColor