tests
testProcessorsFiltering

	spotter filterByMethods: {
		GTSpotter>>#spotterImplementorsFor:.
		GTSpotter>>#spotterForPragmasFor:}.

	self shouldnt: [ spotter setText: '' from: self ] raise: Error. 
	
	self assertSearch: ' '.
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorByTitle: 'Implementors'.

	self assert: currentStep processors size equals: 2.
	