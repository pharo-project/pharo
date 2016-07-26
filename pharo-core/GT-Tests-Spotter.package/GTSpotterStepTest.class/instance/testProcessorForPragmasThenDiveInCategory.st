tests
testProcessorForPragmasThenDiveInCategory
	self testProcessorForPragmas.
	
	" diving into the category (without search) just reveals what was shown before - so this can be repeated infinitely and should return the same result each time "
	4 timesRepeat: [
		self assertDiveInCategory.
		self assertProcessorByTitle: 'Pragmas'.
		self assertProcessorNotEmpty.
		self assert: currentProcessor allFilteredCandidates size >= 1.
		self assertCandidateMatching: [ :each | each class = PragmaType and: [ each keyword = #spotterOrder: ] ]  ].
	
	4 timesRepeat: [
		self assert: currentStep context text = ''.
		self assertDiveOut ].
	
	self assert: currentStep context text = 'spotterOrder:'