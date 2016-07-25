opening
openOnPackage: selectedPackage class: selectedClass category: selectedCategory method: selectedMethod group: selectedGroup sortHierarchically: sortHierarchically showHierarchy: showHierarchy showComment: showComment showInstance: showInstance showCategories: showCategories inEnvironment: anEnvironment
	| instance |
	instance := self new.
	instance browsedEnvironment: anEnvironment.
	instance
		package: selectedPackage
		class: selectedClass
		protocol: selectedCategory
		method: selectedMethod.
	instance selectedGroup: selectedGroup.
	instance
		sortHierarchically: sortHierarchically;
		showComment: showComment;
		showInstance: showInstance;
		showCategories: showCategories;
		showHierarchy: showHierarchy.
	instance open.
	instance ui
		showHierarchy: showHierarchy;
		showComment: showComment.
	^ instance