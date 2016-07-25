editing keys
offerFontMenu: aKeyboardEvent 
	"The user typed the command key that requests a font change; Offer the font menu."

	self closeTypeIn.
	self offerFontMenu.
	^ true