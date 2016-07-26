*Polymorph-Widgets
navigateFocusForward
	"Change the keyboard focus to the next morph or the receiver
	in none are interested."

	self nextMorphWantingFocus
		ifNil: [self takeKeyboardFocus]
		ifNotNil: [super navigateFocusForward]
	