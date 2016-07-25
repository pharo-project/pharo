triggering
classCategoryRenamedFrom: anOldClassCategoryName to: aNewClassCategoryName

	self announce: (CategoryRenamed
					classCategoryRenamedFrom: anOldClassCategoryName 
					to: aNewClassCategoryName)