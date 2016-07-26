menus behavior
removeEmptyCategories

	self okToChange ifFalse: [^ self].
	(self methodsForCategory: self selectedCategory) 
		ifEmpty: [ self selectedCategory: nil ].
		
	self removeEmptyCategoriesFrom: self selectedClass.
	
	self updateCategoryAndMethodList.