api
show

	self widget ifNotNil: [:widget |
		(widget respondsTo: #show) ifTrue: [ widget show ]].