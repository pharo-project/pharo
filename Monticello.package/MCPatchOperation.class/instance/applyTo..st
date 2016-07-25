applying operation
applyTo: anObject

	(self isConflict and: [self isResolved not] ) ifTrue: [
		self error: 'Cannot continue until this conflict has been resolved' ].
	
	self remoteChosen ifTrue: [ self basicApplyTo: anObject ].