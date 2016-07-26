tests
testReorderingWithRemovals
	|dec cats newCats |
	dec := MCOrganizationDefinition categories: #(A B C).
	cats := #(X Y B Z C A Q).
	newCats := dec reorderCategories: cats original: #(Y B C A Q).
	self assert: newCats asArray = #(X A B C Z).