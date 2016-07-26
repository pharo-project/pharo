tests
testProcessorForClasses
	self assertSearch: 'Collection'.
	self assertProcessorByTitle: 'Classes'.
	self assertProcessorNotEmpty.
	self assertProcessorIncludesAll: (self subclassesOf: Collection 
														filterBySubstring: 'Collection' 
														inPackage: 'Collections-Abstract')