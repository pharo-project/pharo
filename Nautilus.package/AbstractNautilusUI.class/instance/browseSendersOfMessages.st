menus behavior
browseSendersOfMessages
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self selectedMethod ifNotNil: [ :met | self browseSendersOfMessagesFrom: met selector ]