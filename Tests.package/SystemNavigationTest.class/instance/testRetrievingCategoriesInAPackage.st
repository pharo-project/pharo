testing
testRetrievingCategoriesInAPackage
	| retrievedCategories |
	
	#(#CategoryForTest #'CategoryForTest-Core' #'CategoryForTest-Tests' #CategoryForTest #ForTestCategory #'ForTestCategory-Core') do: [ :catName | 
		self classFactory newClassInCategory: catName].
	
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryForTest'.
	self
		assert: (retrievedCategories includesAllOf: #(#CategoryForTest #'CategoryForTest-Core' #'CategoryForTest-Tests' )).
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryForTest-Core'.
	self
		assert: (retrievedCategories includesAllOf: #(#'CategoryForTest-Core')). 
	retrievedCategories := self systemNavigationToTest categoriesInPackageNamed: 'CategoryFor'.
	self assert: retrievedCategories isEmpty.