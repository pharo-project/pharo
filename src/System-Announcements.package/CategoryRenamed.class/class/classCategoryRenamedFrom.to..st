instance creation
classCategoryRenamedFrom: anOldCategoryName to: aNewCategoryName
	^self new
			newCategoryName: aNewCategoryName;
			oldCategoryName: anOldCategoryName;
			yourself