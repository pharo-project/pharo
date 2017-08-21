settings
showWarning
	"If true, then a message is send to the Transcript for each deprecated method invocation"
	^ ShowWarning ifNil: [ShowWarning := true]