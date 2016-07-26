selection
deselectAll

	self isMultipleSelection ifFalse: [ ^ self ].
	self resetListSelection