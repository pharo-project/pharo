tests
testReordering
	|dec cats newCats |
	dec := MCOrganizationDefinition categories: #(A B C).
	cats := #(X Y B Z C A Q).
	newCats := dec reorderCategories: cats original: #(B C A).
	self assert: newCats asArray = #(X Y A B C Z Q).