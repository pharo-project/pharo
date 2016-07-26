style
pressedThumbFillStyle
	"Return the pressed thumb fillStyle for the receiver."
	
	^(self theme scrollbarPressedThumbFillStyleFor: self) asColor darker