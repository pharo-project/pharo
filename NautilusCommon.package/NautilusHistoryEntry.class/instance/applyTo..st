protocol
applyTo: aNautilus
	aNautilus ui ifNotNil: [ :interface | interface resetAllListSelections ].
	aNautilus
		selectedPackage: self selectedPackage;
		selectedClass: selectedClass;
		selectedCategory: self selectedCategory.
	aNautilus selectedMethod: self selectedMethod.
	aNautilus browsingHistory
		pauseDuring: [ 
			aNautilus
				selectedGroup: self selectedGroup;
				showHierarchy: self showHierarchy;
				sortHierarchically: self sortHierarchically;
				showComment: self showComment;
				showInstance: self showInstance;
				showCategories: self showCategories ].
	aNautilus ui
		ifNotNil: [ :interface | 
			interface showHierarchy: aNautilus showHierarchy.
			interface showInstance: (selectedClass ifNil: [ false ] ifNotNil: [ selectedClass isMeta not ]).
			interface
				selectedPackageWithoutChangingSelectionInternally: self selectedPackage;
				selectedClassWithoutChangingSelectionInternally: selectedClass;
				selectedCategoryInternally: self selectedCategory.
			interface updatePackageGroupAndClassList.
			interface selectedMethod: self selectedMethod.
			interface updateCategoryAndMethodList ]