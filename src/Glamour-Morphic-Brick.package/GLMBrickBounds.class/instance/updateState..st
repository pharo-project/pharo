private
updateState: aRectangle

	self brickBounds width = aRectangle width ifFalse: [ self markWidthDirty ].
	self brickBounds height = aRectangle height ifFalse: [ self markHeightDirty ]