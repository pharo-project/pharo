*Polymorph-Widgets
hasKeyboardFocus
	"Answer whether the receiver has keyboard focus."

	^((self world ifNil: [^false]) 
		activeHand ifNil: [^false])  keyboardFocus = self