accessing
messageText
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]