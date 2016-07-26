event handling
mouseLeave: anEvent

	anEvent hand showTemporaryCursor: nil.
	self setDefaultColors.
	self shouldInvalidateOnMouseTransition ifTrue: [self changed]. 
	"avoid unnecessary invalidation"