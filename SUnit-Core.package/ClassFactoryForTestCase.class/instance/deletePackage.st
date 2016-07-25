cleaning
deletePackage
	| categoriesMatchString |
	categoriesMatchString := self packageName, '-*'.
	SystemOrganization removeCategoriesMatching: categoriesMatchString