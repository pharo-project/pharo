tests
testProcessorForImplementors
	| partialImplementors |
	self assertSearch: 'do:'.
	self assertProcessorByTitle: 'Implementors'.
	self assertProcessorNotEmpty.
	
	partialImplementors := (SystemNavigation default allImplementorsOf: #do:) collect: [ :rm | rm compiledMethod ].
	self assertProcessorIncludesAll: partialImplementors.
	self assert: currentProcessor allFilteredCandidates size > partialImplementors size.
	
	self assertSearch: 'size'.
	self assertProcessorByTitle: 'Implementors'.
	self assertProcessorNotEmpty.
	
	partialImplementors := (SystemNavigation default allImplementorsOf: #size) collect: [ :rm | rm compiledMethod ].
	self assertProcessorIncludesAll: partialImplementors.
	self assert: currentProcessor allFilteredCandidates size > partialImplementors size.