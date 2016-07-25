testing
willCrashTheVMtestLibCExamples
	| sortedData unsorted sorted |
	
	sortedData := Alien exampleCqsort.
	unsorted := sortedData key copy.
	sorted := unsorted asSortedCollection.
	self should: [sortedData value = sorted asArray].
	Alien examplePrintf.