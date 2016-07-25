event handling
windowEvent: anEvent
	self windowEventHandler ifNotNil: [ ^ self windowEventHandler windowEvent: anEvent ].
	"The window close dialog is modal, since the windowClose event originates from the system window button, we need to protect against opening multiple, otherwise we end up with the world in a locked state when one of them is cancelled"
	(anEvent type == #windowClose and: [ self valueOfProperty: #canOpenCloseDialog ifAbsent: true ])
		ifTrue: [ 
			self setProperty: #canOpenCloseDialog toValue: false.
			WorldState quitSession.
			self removeProperty: #canOpenCloseDialog ]