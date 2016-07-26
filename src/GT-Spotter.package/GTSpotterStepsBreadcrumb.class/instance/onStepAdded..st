events-processing
onStepAdded: ann
	|step origin previousStep selectedCandidate|
	
	self spotterModel steps size <= 1 ifTrue: [
		^ self push: (GTSpotterStepBreadcrumbItem new
						text: self spotterModel steps first origin gtDisplayString;
						stepModel: self spotterModel steps first) ].
	
	step := ann stepModel.
	previousStep := (step spotter steps at: step spotter steps size - 1).
	selectedCandidate := previousStep selected candidate.
	origin := step origin.
	
	self push: (GTSpotterStepBreadcrumbItem new
		stepModel: step;
		text: ((selectedCandidate = origin)
			ifTrue: [ (previousStep selected renderingProcessor render: selectedCandidate) labelBrick text ]
			ifFalse: [ (origin isKindOf: Collection)
				ifTrue: [ step processors first title ]
				ifFalse: [ origin gtDisplayString] ]))