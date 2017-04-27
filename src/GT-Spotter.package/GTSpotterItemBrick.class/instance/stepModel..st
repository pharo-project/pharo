accessing
stepModel: aModel

	stepModel := aModel.
	
	stepModel announcer when: GTSpotterSelectedCandidateChanged send: #onSelectedChanged: to: self.
	stepModel spotter announcer when: GTSpotterStepPopped send: #onStepPopped to: self.
	
	self actionbarBrick stepModel: aModel.
	
	self
		highlightQuery;
		checkOrUncheck