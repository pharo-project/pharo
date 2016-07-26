actions
selectPreviousCategory

	self spotterModel hasSelected
		ifTrue: [ self spotterModel currentStep selectPreviousCategory ]
	