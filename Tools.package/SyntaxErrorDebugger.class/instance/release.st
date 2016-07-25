initialization
release
	"Prevent that syntaxError window lets a suspended compiler process running, when the window is closed."

	debugSession ifNotNil: [ debugSession terminate ].
	self releaseActionMap. "we are not sure if we need it"
	super release.