actions
selectNextCandidateDueTo: anEventType

	self spotterModel hasSelected
		ifTrue: [ self spotterModel currentStep selectNextDueTo: anEventType ]