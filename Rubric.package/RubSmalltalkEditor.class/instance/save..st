editing keys
save: aKeyboardEvent
	"Submit the current text.  Equivalent to 'accept' 1/18/96 sw"

	self closeTypeIn.
	self accept.
	^ true