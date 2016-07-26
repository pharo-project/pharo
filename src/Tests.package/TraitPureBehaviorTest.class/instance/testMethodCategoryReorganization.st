testing
testMethodCategoryReorganization
	"self run: #testMethodCategory"

	self t1 compile: 'm1' classified: 'category1'.
	self assert: (self t5 organization categoryOfElement: #m1) = #category1.
	self assert: (self c2 organization categoryOfElement: #m1) = #category1.
	self t1 organization 
		classify: #m1
		under: #category2
		suppressIfDefault: true.
	self assert: (self t5 organization categoryOfElement: #m1) = #category2.
	self assert: (self c2 organization categoryOfElement: #m1) = #category2