events-processing
mouseStillDown: anEvent
	(anEvent hand position y > self innerBounds bottom)
		ifTrue: [self listManager selectMoreAtBottom]
		ifFalse: [
			(anEvent hand position y < self innerBounds top)
				ifTrue: [self listManager selectMoreAtTop]
				ifFalse: [super mouseStillDown: anEvent]]