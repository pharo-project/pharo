actions
browseSendersOfMessages
	"Present a menu of the current message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self currentMessageName ifNotNil: [   
		self systemNavigation browseAllSendersOf: self currentMessageName ]