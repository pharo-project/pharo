stepping and presenter
stepAt: millisecondClockValue
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.
	The millisecondClockValue parameter gives the value of the millisecond clock at the moment of dispatch.
	Default is to dispatch to the parameterless step method for the morph, but this protocol makes it possible for some morphs to do differing things depending on the clock value"

	self step
