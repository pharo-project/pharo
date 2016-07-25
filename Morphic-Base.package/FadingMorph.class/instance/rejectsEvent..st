events-processing
rejectsEvent: anEvent

	^(anEvent isMouse and: [ anEvent isMouseDown ]) or: [ super rejectsEvent: anEvent ]