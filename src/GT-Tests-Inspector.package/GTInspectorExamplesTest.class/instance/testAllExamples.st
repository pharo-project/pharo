tests
testAllExamples
	self allExamplesClasses
		do: [ :eachClass | 
			eachClass gtExamples do: [ :exampleToTest | 
				exampleToTest ignoreTest ifFalse: [ 
					self checkAllPresentationsOf: exampleToTest returnValue] ] ]