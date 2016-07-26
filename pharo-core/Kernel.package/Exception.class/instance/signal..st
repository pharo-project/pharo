signaling
signal: signalerText
	"Signal the occurrence of an exceptional condition with a specified textual description."

	self messageText: signalerText.
	^ self signal