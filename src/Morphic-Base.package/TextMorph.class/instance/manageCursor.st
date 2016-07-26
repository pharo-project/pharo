blinking
manageCursor
	(paragraph isNil or: [paragraph focused not])
		ifTrue: [^ self
				resetBlinkCursor;
				stopBlinking].
	Editor blinkingCursor 
		ifTrue: [self startBlinking]
		ifFalse: [self ensureCursor]