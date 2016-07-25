convenience
assertDiveOut
	| previousStepsSize |
	" backup previous conditions "
	previousStepsSize := spotter steps size.
	previousStep := spotter currentStep.

	" perform the dive operation - like the UI would do "
	self shouldnt: [ spotter popStep ] raise: Error.
	currentStep := spotter currentStep.
	currentProcessor := currentCandidate := previousContext := currentContext := nil.
	
	self assert: spotter steps size < previousStepsSize.
	self assert: spotter steps size = (previousStepsSize - 1).
	
	self deny: (previousStepsSize > 1 and: [ previousStep = currentStep ]).
	self deny: (previousStepsSize > 1 and: [ previousStep = spotter currentStep ]).
	self assert: currentStep = spotter currentStep.