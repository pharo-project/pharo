*Morphic-Base
startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime
	"Add the given morph to the step list. Do nothing if it is already being stepped."

	self stopStepping: aMorph selector: aSelector.
	self adjustWakeupTimesIfNecessary.
	stepList add:(
		StepMessage 
			scheduledAt: scheduledTime
			stepTime: stepTime
			receiver: aMorph
			selector: aSelector
			arguments: args)