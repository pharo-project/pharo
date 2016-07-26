event handling
onStepAdded
	
	self selectedItemBrick: nil.
	self spotterModel steps last candidates announcer when: GTSpotterAllCandidatesRemoved send: #onAllCandidatesRemoved to: self.