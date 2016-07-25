accessing
candidates

	^ candidates ifNil: [
		candidates := GTSpotterCandidatesList new.
		candidates announcer subscribe: GTSpotterCandidateAdded send: #onCandidateAdded to: self.
		candidates announcer subscribe: GTSpotterAllCandidatesAdded send: #onCandidateAdded to: self.
		candidates announcer subscribe: GTSpotterAllCandidatesRemoved do: [ self deselect ].
		candidates ]