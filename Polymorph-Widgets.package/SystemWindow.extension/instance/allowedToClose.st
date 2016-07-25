*Polymorph-Widgets
allowedToClose
	"Answer whether the window is currently allowed to close."

	^self mustNotClose not and: [
		self modalChild isNil]