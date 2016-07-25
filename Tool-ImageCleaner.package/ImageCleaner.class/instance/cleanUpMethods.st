cleaning
cleanUpMethods
	"Make sure that all methods in use are restarted"
	"self new cleanUpMethods"

	WeakArray restartFinalizationProcess.
	WorldState allInstancesDo: [ :ws | ws convertAlarms; cleanStepList; resetWorldMenu].
	WorldState allInstancesDo: [ :ws | ws instVarNamed: 'menuBuilder' put: nil ].
	ProcessBrowser initialize.
	Delay restartTimerEventLoop.