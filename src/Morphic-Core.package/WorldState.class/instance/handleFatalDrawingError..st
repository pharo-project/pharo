update cycle
handleFatalDrawingError: errMsg
	"Handle a fatal drawing error."
	Display deferUpdates: false. "Just in case"
	self primitiveError: errMsg.

	"Hm... we should jump into a 'safe' worldState here, but how do we find it?!"