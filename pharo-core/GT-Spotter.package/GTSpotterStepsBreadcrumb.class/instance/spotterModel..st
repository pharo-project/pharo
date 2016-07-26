accessing
spotterModel: aSpotterModel
	
	spotterModel := aSpotterModel.
	
	spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded: to: self.
	spotterModel announcer when: GTSpotterStepPopped send: #onStepPopped to: self.