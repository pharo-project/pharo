accessing
hasKeyboardFocus
	"Answer whether the receiver has keyboard focus."

	^ super hasKeyboardFocus or: [(self textArea ifNil: [^false]) hasKeyboardFocus]