stepping
amendSteppingStatus
	"Circumstances having changed, find out whether stepping is wanted and assure that the new policy is carried out"

	self wantsSteps
		ifTrue:
			[self arrangeToStartStepping]
		ifFalse:
			[self stopStepping]