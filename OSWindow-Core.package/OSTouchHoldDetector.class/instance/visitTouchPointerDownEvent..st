visiting
visitTouchPointerDownEvent: anEvent
	super visitTouchPointerDownEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers
		ifTrue: [  process := self createWaitingProcess ]
		ifFalse: [ process terminate ]