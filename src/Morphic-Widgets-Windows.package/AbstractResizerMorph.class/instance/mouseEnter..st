event handling
mouseEnter: anEvent

	self isCursorOverHandle ifTrue:
		[self setInverseColors.
		self shouldInvalidateOnMouseTransition ifTrue: [self changed]. "avoid unnecessary invalidation"
		anEvent hand showTemporaryCursor: self resizeCursor]