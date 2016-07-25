test shadowing
testTraitTempShadowing

	self initializeErrorMessage.

	OpalCompiler new 
	source: 'testReplaceFromToWithStartingAt
	| result  repStart collection replacementCollection firstIndex secondIndex |
		replacementCollection := 1.';
	class: ArrayTest;
	failBlock: [self fail.];
	translate.
	self assert: ( newTranscript contents =  '
ArrayTest>>testReplaceFromToWithStartingAt(replacementCollection is shadowed)
ArrayTest>>testReplaceFromToWithStartingAt(firstIndex is shadowed)
ArrayTest>>testReplaceFromToWithStartingAt(secondIndex is shadowed)').