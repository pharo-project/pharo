accessing
message: aString
	"Set a message in the pre debug window. 
	If this is nil the notifier pane will display a text editor showing the message (Warning signal: 'Display this text'),
	otherwise the stack will be displayed."
	
	message := aString