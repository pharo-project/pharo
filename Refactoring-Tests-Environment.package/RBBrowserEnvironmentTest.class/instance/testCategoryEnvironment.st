testing-environments
testCategoryEnvironment
	| aCategoryEnvironment |
	self skip: 'Use RBPackageEnvironment instead.'.
	aCategoryEnvironment := RBCategoryEnvironment 
				onEnvironment: RBBrowserEnvironment new
				categories: #(#'Kernel-Objects').
	self universalTestFor: aCategoryEnvironment.
	self assert: (aCategoryEnvironment implementorsOf: #printString) numberSelectors equals: 1