accessing
spotterModel: aSpotterModel

	spotterModel := aSpotterModel.
	
	self spotterModel announcer when: GTSpotterCandidateSelected send: #onCandidateSelected: to: self.
	self spotterModel announcer when: GTSpotterCandidateSelectedMoved send: #onCandidateMoved to: self.
	self spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded to: self.
	self spotterModel announcer when: GTSpotterShowPreview send: #onShowPreview to: self.
	self spotterModel announcer when: GTSpotterHidePreview send: #onHidePreview to: self.
	
	self spotterModel announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self spotterModel announcer when: GTSpotterHideHints send: #onHideHints to: self.