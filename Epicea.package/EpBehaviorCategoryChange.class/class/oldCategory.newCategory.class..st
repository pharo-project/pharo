instance creation
oldCategory: anOldCategory newCategory: aNewCategory class: aClass

	^ self basicNew 
		initializeOldCategory: anOldCategory newCategory: aNewCategory class: aClass;
		yourself