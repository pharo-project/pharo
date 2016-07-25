events handling
keyStroke: event
	self selectedNode ifNotNil: [:node | node keyStroke: event]