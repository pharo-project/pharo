api
hide

	self widget ifNotNil: [:widget |
		(widget respondsTo: #hide) ifTrue: [ widget hide ]].