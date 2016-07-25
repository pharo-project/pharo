actions
selectNextCategory

	self spotterModel hasSelected
		ifTrue: [ self spotterModel currentStep selectNextCategory ]
	