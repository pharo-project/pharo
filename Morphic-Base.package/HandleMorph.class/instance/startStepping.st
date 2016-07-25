stepping and presenter
startStepping
	"Make the receiver the keyboard focus for editing"
	super startStepping.
	"owner isHandMorph ifTrue:[owner newKeyboardFocus: self]."
self flag: #arNote. "make me #handleKeyboard:"