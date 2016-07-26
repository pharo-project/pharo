testing
testTraitCreationInDifferentCategories
	| firstThreeTraits lastTwoTraits |
	3 timesRepeat: [
		factory newTraitInCategory: #One].
	firstThreeTraits := factory createdTraits copy.
	2 timesRepeat: [
		factory newTraitInCategory: #Two].
	lastTwoTraits := factory createdTraits copyWithoutAll: firstThreeTraits.
	self assert: (firstThreeTraits allSatisfy: [:trait| trait category = (factory packageName, '-', #One) asSymbol]).
	self assert: (lastTwoTraits allSatisfy: [:trait| trait category = (factory packageName, '-', #Two) asSymbol]).