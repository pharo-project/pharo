focus handling
releaseKeyboardFocus: aMorph
	"If the given morph had the keyboard focus before, release it"
	self keyboardFocus == aMorph ifTrue:[self releaseKeyboardFocus].