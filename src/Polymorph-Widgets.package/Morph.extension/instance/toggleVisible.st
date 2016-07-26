*Polymorph-Widgets
toggleVisible
	"Toggle the visibility of the receiver."

	self visible
		ifTrue: [self hide]
		ifFalse: [self show]