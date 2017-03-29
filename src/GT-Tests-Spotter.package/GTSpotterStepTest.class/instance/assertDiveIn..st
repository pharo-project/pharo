private
assertDiveIn: aSelector
	| previousStepsSize link |
	" backup previous conditions "
	previousStepsSize := spotter steps size.
	previousStep := spotter currentStep.

	" get and validate the link "
	link := spotter currentStep candidates getCandidateLink: currentCandidate in: currentProcessor.
	
	self assert: link notNil.
	self assert: link candidate == currentCandidate.
	self assert: link processor == currentProcessor.

	" set the selection - like the UI would do "
	self shouldnt: [ spotter currentStep select: link ] raise: Error.
	self assert: spotter currentStep selected == link.
	
	" perform the dive operation - like the UI would do "
	self shouldnt: [ spotter currentStep perform: aSelector ] raise: Error.
	currentStep := spotter currentStep.
	currentProcessor := currentCandidate := previousContext := currentContext := nil.
	
	self deny: spotter steps isEmpty .
	self assert: spotter steps size > previousStepsSize.
	self assert: spotter steps size = (previousStepsSize + 1).
	
	self deny: previousStep = currentStep.
	self deny: previousStep = spotter currentStep.
	self assert: currentStep = spotter currentStep.