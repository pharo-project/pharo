event handling
mouseUp: evt
	super mouseUp: evt.
	oldColor ifNotNil:
		["if oldColor nil, it signals that mouse had not gone DOWN
		inside me, e.g. because of a cmd-drag; in this case we want
		to avoid triggering the action!"
		self color: oldColor.
		oldColor := nil.
		(self containsPoint: evt cursorPoint) 
				ifTrue: [ actWhen == #buttonUp 
							ifTrue: [self doButtonAction]  ]
				ifFalse: [ self mouseLeave: evt "This is a balk. Note that we have left." ]]
		
