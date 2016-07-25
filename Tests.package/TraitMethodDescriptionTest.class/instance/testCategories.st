testing
testCategories
	self assert: (self t4 organization categoryOfElement: #m21) equals: #cat1.
	self assert: (self t4 organization categoryOfElement: #m22) equals: #cat2.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t4 organization categoryOfElement: #m12) equals: #cat2.
	self assert: (self t4 organization categoryOfElement: #m13) equals: #cat3.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #cat2.
	self t2 organization classify: #m22 under: #catX.
	self assert: (self t4 organization categoryOfElement: #m22) equals: #catX.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #catX.
	self t6 organization classify: #m22 under: #catY.
	self t6 organization classify: #m22Alias under: #catY.
	self t2 organization classify: #m22 under: #catZ.
	self assert: (self t6 organization categoryOfElement: #m22) equals: #catY.
	self assert: (self t6 organization categoryOfElement: #m22Alias) equals: #catY.
	self t1 compile: 'mA' classified: #catA.
	self assert: (self t4 organization categoryOfElement: #mA) equals: #catA.
	self t1 organization classify: #mA under: #cat1.
	self assert: (self t4 organization categories includes: #catA) not