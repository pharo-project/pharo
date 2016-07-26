spec accessing
selector: aSymbol
	"set the selector for item with an action"
	self spec action 
		ifNil: [self spec action: (MessageSend receiver: self itemReceiver selector: aSymbol)]
		ifNotNil: [self spec action selector: aSymbol]