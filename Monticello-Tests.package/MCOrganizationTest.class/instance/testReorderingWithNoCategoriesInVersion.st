tests
testReorderingWithNoCategoriesInVersion
	|dec cats newCats |
	dec := MCOrganizationDefinition categories: #().
	cats := #(X Y B Z C A Q).
	newCats := dec reorderCategories: cats original: #().
	self assert: newCats asArray = cats.