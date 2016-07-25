focus handling
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	keyboardFocus == aMorphOrNil ifTrue: [ ^self ].
	oldFocus := keyboardFocus.
	keyboardFocus := aMorphOrNil.
	
	oldFocus ifNotNil: [oldFocus keyboardFocusChange: false].
	aMorphOrNil ifNotNil: [ aMorphOrNil keyboardFocusChange: true ].
