actions
selectPreviousCandidateDueTo: anEventType

	self spotterModel hasSelected
		ifTrue: [ self spotterModel currentStep selectPreviousDueTo: anEventType ]
	