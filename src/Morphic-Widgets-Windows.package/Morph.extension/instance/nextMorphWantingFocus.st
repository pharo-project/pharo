*Morphic-Widgets-Windows
nextMorphWantingFocus
	"Answer the next morph that wants keyboard focus."

	|m|
	m := self nextMorphInWindow ifNil: [^nil].
	[m = self or: [m wantsKeyboardFocusNavigation]]
		whileFalse: [m := m nextMorphInWindow].
	^m wantsKeyboardFocusNavigation
		ifTrue: [m]
	