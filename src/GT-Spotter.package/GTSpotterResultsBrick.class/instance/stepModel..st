accessing
stepModel: aModel
	stepModel := aModel.
	
	stepModel candidates announcer when: GTSpotterCandidateAdded send: #onCandidateAdded: to: self.
	stepModel candidates announcer when: GTSpotterAllCandidatesRemoved send: #onAllCandidatesRemoved to: self.
	stepModel candidates announcer when: GTSpotterAllCandidatesAdded send: #onAllCandidatesAdded: to: self.
	stepModel candidates announcer when: GTSpotterCandidatesAmountChanged send: #onCandidatesAmountChanged: to: self.
	stepModel announcer when: GTSpotterScrollPageDown send: #scrollPageDown to: self.
	stepModel announcer when: GTSpotterScrollPageUp send: #scrollPageUp to: self.
	stepModel announcer when: GTSpotterCandidateSelected send: #onCandidateSelected: to: self