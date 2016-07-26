initialize
copyHandlerState: anEvent
	"Copy the handler state from anEvent. Used for quickly transferring handler information between transformed events."
	handler := anEvent handler.
	wasHandled := anEvent wasHandled.