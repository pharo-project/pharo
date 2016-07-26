testing
containedBy: anInterval 
	^anInterval first <= self start and: [anInterval last >= self stop]