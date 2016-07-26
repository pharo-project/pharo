testing
mayActOnEvent: anEvent
	^ anEvent isMouseUp and: [ anEvent commandKeyPressed ]