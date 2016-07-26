*Polymorph-Widgets
hasKeyboardFocus
	"Answer whether the receiver has keyboard focus."

	^super hasKeyboardFocus or: [(self textMorph ifNil: [^false]) hasKeyboardFocus]