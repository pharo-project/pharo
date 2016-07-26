editing
handleDeleteKey

	(controller selection contains: (self layoutView layout startPosition)) ifTrue: [ 
		"The selected portion to delete contains layout starting position. We must
		take care resetting view to new position"
		self layoutView resetLayout: controller selection start.
		 ].

	controller delete.
	self controllerChanged.