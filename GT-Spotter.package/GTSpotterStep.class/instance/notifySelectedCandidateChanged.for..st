notifying
notifySelectedCandidateChanged: aDoubleLink for: anEventType
	|candidate|
	
	candidate := aDoubleLink ifNotNil: [ :o | o value ].
	self announcer announce: (GTSpotterSelectedCandidateChanged new selected: candidate).
	self spotter notifyCandidateSelected: candidate for: anEventType