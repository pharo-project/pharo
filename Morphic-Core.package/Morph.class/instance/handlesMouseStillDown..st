event handling
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	self eventHandler ifNotNil: [^ self eventHandler handlesMouseStillDown: evt].
	^ false
