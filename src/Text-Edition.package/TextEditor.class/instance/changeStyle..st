typing/selecting keys
changeStyle: aKeyboardEvent 
	"Put up the style-change menu"

	self closeTypeIn.
	self changeStyle.
	^ true