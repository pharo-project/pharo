testing
testUsers
	self assert: self t2 classSide traitUsers size = 3.
	self assert: (self t2 classSide traitUsers includesAll: {				
		(self t4 classTrait).
		(self t5 classTrait).
		(self t6 classTrait) }).
	self assert: self t5 classSide traitUsers size = 1.
	self assert: self t5 classSide traitUsers anyOne = self c2 class.
	self c2 setTraitCompositionFrom: self t1 + self t5.
	self assert: self t5 classSide traitUsers size = 1.
	self assert: self t5 classSide traitUsers anyOne = self c2 class.
	self c2 setTraitComposition: self t2 asTraitComposition.
	self assert: self t5 classSide traitUsers isEmpty