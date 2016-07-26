*Polymorph-Widgets
toggleVisibleAndRaise
	"Toggle the visibility of the receiver, brining to
	the front if becoming visible."

	self visible
		ifTrue: [self hide]
		ifFalse: [self comeToFront; show]