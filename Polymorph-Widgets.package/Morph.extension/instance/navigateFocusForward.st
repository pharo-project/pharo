*Polymorph-Widgets
navigateFocusForward
	"Change the keyboard focus to the next morph."

	self nextMorphWantingFocus ifNotNil: [:m |
		m takeKeyboardFocus]
	