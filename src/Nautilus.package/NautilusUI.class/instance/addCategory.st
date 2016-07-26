menus behavior
addCategory
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| newName |
	
	self okToChange ifFalse: [^ self].
	self selectedClass ifNil: [^ self].

	newName := self 
		addCategoryIn: self selectedClass 
		before: self selectedCategory.
	self resetCategoriesListSelection.
	categoryWidget selectProtocol: newName.
	self selectedCategory: newName.
	newName ifNil: [ ^ self ].
	self 
		updateCategoryAndMethodList;
		giveFocusToSourceCodeArea.