spec accessing
target: anObject
	"set the target for the action"
	self spec action 
		ifNil: [self spec action: (MessageSend receiver: (anObject ifNil: [owner itemReceiver]) selector: nil)]
		ifNotNil: [self spec action receiver: anObject]