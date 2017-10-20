settings
raiseWarning
	"If true, then a dialog is popup for each deprecated method invocation"
	^ RaiseWarning ifNil: [RaiseWarning := true]