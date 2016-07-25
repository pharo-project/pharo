editing keys
paste: aKeyboardEvent 
	"Replace the current text selection by the text in the shared buffer."

	self closeTypeIn.
	self paste.
	^true