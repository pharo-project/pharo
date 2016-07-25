spec accessing
arguments: anArray
	"set the arguments for an item with an action"
	self spec action 
		ifNil: [self spec action: (MessageSend receiver: self itemReceiver selector: nil arguments: anArray)]
		ifNotNil: [self spec action arguments: anArray]