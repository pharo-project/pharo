*Morphic-Widgets-Windows
modelWakeUpIn: aWindow
	"I am the model of a SystemWindow, that has just been activated"

	self viewBox = aWindow panelRect ifFalse: [self viewBox: aWindow panelRect]