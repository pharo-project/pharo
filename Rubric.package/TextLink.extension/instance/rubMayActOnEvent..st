*Rubric-Editing-Core
rubMayActOnEvent: anEvent
	^ anEvent isMouseDown and: [ anEvent commandKeyPressed ]