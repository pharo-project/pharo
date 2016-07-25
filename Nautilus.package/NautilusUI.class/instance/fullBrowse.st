menus behavior
fullBrowse
	| ret |
	ret := self model class
		openOnPackage: self selectedPackage
		class: self selectedClass
		category: self selectedCategory
		method: self selectedMethod
		group: self selectedGroup
		sortHierarchically: self sortHierarchically
		showHierarchy: self showHierarchy
		showComment: self showComment
		showInstance: self showInstance
		showCategories: true.
		
	ret packagePatternString: self packagePatternString.
	^ret.