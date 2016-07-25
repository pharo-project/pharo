accessing
spotterModel: aSpotter
	
	spotterModel := aSpotter.
	aSpotter announcer when: GTSpotterSelectedCandidateChanged send: #onCandidateSelected: to: self.
	aSpotter announcer when: GTSpotterStepPopped send: #onStepPopped to: self.
	aSpotter announcer when: GTSpotterStepAdded send: #onStepAdded to: self.
	aSpotter announcer when: GTSpotterSearchFinished send: #onSearchFinished to: self.
	
	