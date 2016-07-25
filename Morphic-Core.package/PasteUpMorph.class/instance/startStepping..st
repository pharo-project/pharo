stepping
startStepping: aMorph
	"Add the given morph to the step list. Do nothing if it is already being stepped."
	^self startStepping: aMorph at: Time millisecondClockValue selector: #stepAt: arguments: nil stepTime: nil