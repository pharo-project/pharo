*Polymorph-Widgets
navigateFocusBackward
	"Change the keyboard focus to the previous morph."

	self previousMorphWantingFocus ifNotNil: [:m |
		m takeKeyboardFocus]
	