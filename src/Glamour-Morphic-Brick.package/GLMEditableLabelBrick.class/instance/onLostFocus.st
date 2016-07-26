label-events
onLostFocus
	
	self isCancelOnFocusLost & self isEditMode
		ifTrue: [ self cancel ].
	
	self isEditMode
		ifTrue: [ self animateToEditNotFocused ]
		ifFalse: [ self animateToNormal ]