events-processing
handleListenEvent: anEvent

	self isHideUnhover not & (anEvent type = #mouseMove)
		ifTrue: [ ^ self ].

	(self isInsideOfPopupBrick: anEvent) ifFalse: [ self close ]