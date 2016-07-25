actions
diveInDueTo: anEventType
	self canDiveInSelectedCandidate ifTrue: [ 
		self spotter
			historize: self selected;
			requestNextStepWith: self selected candidate for: #diveIn;
			notifyDiveIn: self selected candidate for: anEventType ]