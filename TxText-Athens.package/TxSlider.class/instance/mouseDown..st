event handling
mouseDown: evt

	| downPos |
	
	downPos := self mapMouseRelativeToSlider: evt position.
	
	downPos < mappedPosition ifTrue: [ ^ self handleMouseDownAheadOfSlider: evt mappedPosition: downPos ].
	downPos > (mappedPosition + mappedThickness) ifTrue: [ ^ self handleMouseDownAfterSlider: evt  mappedPosition: downPos].
	
	self handleMouseDownOnSlider: evt  mappedPosition: downPos