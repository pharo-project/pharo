*Polymorph-Widgets
toggleVisibleAndRaise
	"Toggle the visibility of the receiver, bringing to
	the front if becoming visible. Activate or passivate here."

	self isActive ifTrue: [self world navigateVisibleWindowForward].
	super toggleVisibleAndRaise.
	self visible ifTrue: [self activate]