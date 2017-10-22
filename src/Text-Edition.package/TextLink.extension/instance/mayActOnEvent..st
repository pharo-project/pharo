*Text-Edition
mayActOnEvent: anEvent
	^ anEvent isMouseUp and: [ anEvent commandKeyPressed ]