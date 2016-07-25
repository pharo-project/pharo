event handling
processMouseOver: anEvent 

	self initializeProcessMouseOver.
	self handleAsMouseOver: anEvent.
	self hasLeftMorphsChanged ifTrue: [ 
		self handleAsMouseLeave: anEvent.
		self handleAsMouseEnter: anEvent.
		self rememberOverList ].
	
	self initializeTrackedMorphs 
