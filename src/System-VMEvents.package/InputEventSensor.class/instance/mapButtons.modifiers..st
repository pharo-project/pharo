private events
mapButtons: buttons modifiers: anInteger
	"Map the buttons to yellow or blue based on the given modifiers.
	If only the red button is pressed, then map
		Ctrl-RedButton -> BlueButton.
		Cmd-RedButton -> YellowButton.
	"
	(buttons = RedButtonBit)
		ifFalse:[^buttons].
	(anInteger allMask: CtrlKeyBit) 
		ifTrue:[^BlueButtonBit].
	(anInteger allMask: CommandKeyBit) 
		ifTrue:[^YellowButtonBit].
	^buttons