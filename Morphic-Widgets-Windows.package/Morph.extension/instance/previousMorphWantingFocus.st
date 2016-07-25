*Morphic-Widgets-Windows
previousMorphWantingFocus
	"Answer the previous morph that wants keyboard focus."

	|m|
	m := self previousMorphInWindow ifNil: [^nil].
	[m = self or: [m wantsKeyboardFocusNavigation]]
		whileFalse: [m := m previousMorphInWindow ifNil: [^nil]].
	^m wantsKeyboardFocusNavigation
		ifTrue: [m]
	