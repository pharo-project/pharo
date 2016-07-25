accessing
listProcessor
	| processor |
	processor := GTSpotterCandidatesListProcessor new.
	self addProcessor: processor.
	^ processor