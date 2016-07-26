actions
help
	"This is a crude default help implementation."
	self printHelp.
	Smalltalk isInteractive 
		ifFalse: [ self exitSuccess ]