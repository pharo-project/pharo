testing
isDraggingEvent
	source ifNil:[^false].
	source hasSubmorphs ifTrue:[^true].
	self anyButtonPressed ifTrue:[^true].
	^false