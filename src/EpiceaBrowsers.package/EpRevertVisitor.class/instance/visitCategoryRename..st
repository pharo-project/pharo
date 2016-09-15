visitor
visitCategoryRename: aPackageRenamed
	environment organization 
		renameCategory: aPackageRenamed newCategoryName
		toBe: aPackageRenamed oldCategoryName.