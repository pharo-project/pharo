menus behavior
renameCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"

	| category newName vScroll protocol |
	
	self okToChange ifFalse: [^ self].
	
	protocol  := self selectedProtocol. 
	protocol ifNil: [ ^ self ].
	
	protocol canBeRenamed 
		ifFalse: [ ^ self inform: ('You can not rename "{1}" protocol.' format: { protocol name })  ].
	category := self selectedCategory.
	vScroll := categoryWidget vScrollValue.
	newName := self renameCategory: category from: self selectedClass.
	newName ifNil: [ ^ self ].
	categoryWidget deselectProtocol: category.
	self selectedCategory: newName.
	categoryWidget selectProtocol: newName.
	self updateCategoryAndMethodList.
	categoryWidget vScrollValue: vScroll