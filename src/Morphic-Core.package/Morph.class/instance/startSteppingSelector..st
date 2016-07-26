stepping and presenter
startSteppingSelector: aSelector
	"Start getting sent the 'step' message."
	self startStepping: aSelector at: Time millisecondClockValue arguments: nil stepTime: nil.