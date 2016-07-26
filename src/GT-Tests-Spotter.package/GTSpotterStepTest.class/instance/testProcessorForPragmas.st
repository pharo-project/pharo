tests
testProcessorForPragmas
	self assertSearch: 'spotterOrder:'.
	self assertProcessorByTitle: 'Pragmas'.
	self assertProcessorNotEmpty.
	self assert: currentProcessor allFilteredCandidates size >= 1.
	self assertCandidateMatching: [ :each | each class = PragmaType and: [ each keyword = #spotterOrder: ] ]  