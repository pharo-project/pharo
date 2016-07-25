testing
testClassFastCreationInDifferentCategories
	| firstThreeClasses lastTwoClasses |
	3 timesRepeat: [
		factory newClassInCategory: #One].
	firstThreeClasses := factory createdClasses copy.
	2 timesRepeat: [
		factory newClassInCategory: #Two].
	lastTwoClasses := factory createdClasses copyWithoutAll: firstThreeClasses.
	self assert: (firstThreeClasses allSatisfy: [:class| class category = (factory packageName, '-', #One) asSymbol]).
	self assert: (lastTwoClasses allSatisfy: [:class| class category = (factory packageName, '-', #Two) asSymbol]).