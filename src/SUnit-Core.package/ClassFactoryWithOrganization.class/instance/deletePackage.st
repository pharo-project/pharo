cleaning
deletePackage
	| categoriesMatchString |
	categoriesMatchString := self packageName, '-*'.
	self organization removeCategoriesMatching: categoriesMatchString ; 
		removeEmptyCategories
	