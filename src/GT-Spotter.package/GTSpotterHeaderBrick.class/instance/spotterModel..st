accessing
spotterModel: aModel 

	spotterModel := aModel.
	self searchField morph on: spotterModel.

	spotterModel announcer when: GTSpotterCurrentStepChanged send: #onCurrentStepChanged to: self.
	
	self spotterModel announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self spotterModel announcer when: GTSpotterHideHints send: #onHideHints to: self.