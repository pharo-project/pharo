event handling
handlesKeyboard: evt
	"Return true if the receiver wishes to handle the given keyboard event"
	self eventHandler ifNotNil: [^ self eventHandler handlesKeyboard: evt].
	^ false
