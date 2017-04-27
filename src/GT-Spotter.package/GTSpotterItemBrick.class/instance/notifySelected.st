notifying
notifySelected

	self stepModel spotter announce: (GTSpotterCandidateSelected new itemBrick: self).
	self stepModel announcer announce: (GTSpotterCandidateSelected new itemBrick: self)