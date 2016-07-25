accessing
spotterModel: aModel

	spotterModel := aModel.
	self headerBrick spotterModel: spotterModel.
	
	spotterModel steps isEmpty ifFalse: [ spotterModel currentStep hasCandidates ifTrue: [ self showDropDownPane ] ].

	spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded to: self.