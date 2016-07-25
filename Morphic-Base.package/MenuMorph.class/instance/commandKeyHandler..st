accessing
commandKeyHandler: anObject
	"Set the receiver's commandKeyHandler.  Whatever you set here needs to be prepared to respond to the message #commandKeyTypedIntoMenu: "

	self setProperty: #commandKeyHandler toValue: anObject