*Rubric-Editing-Core
rubMayActOnEvent: anEvent
	^ anEvent isMouseMove or: [ anEvent isMouseDown and: [ anEvent commandKeyPressed ] ]