actions
actDueTo: anEventType
	self spotterModelDo: [
		self spotterModel hasSteps ifFalse: [ ^self ].
		self spotterModel currentStep hasCandidates ifFalse: [ ^ self ].
		self spotterModel currentStep actDueTo: anEventType ]