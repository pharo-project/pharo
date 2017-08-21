testing
testUsers
	self assert: self t1 traitUsers size = 3.
	self assert: (self t1 traitUsers includesAll: {self t4. self t5. self t6 }).
	self assert: self t3 traitUsers isEmpty.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 traitUsers anyOne = self c2.
	self c2 setTraitCompositionFrom: self t1 + self t5.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 traitUsers anyOne = self c2.
	self c2 setTraitComposition: self t2 asTraitComposition.
	self assert: self t5 traitUsers isEmpty