private
moveCursor: directionBlock forward: forward specialBlock: specialBlock event: aKeyboardEvent

	super moveCursor: directionBlock forward: forward specialBlock: specialBlock event: aKeyboardEvent.
	self setEmphasisHereFromTextForward: forward