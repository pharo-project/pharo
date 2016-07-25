menus behavior
categorizeMethod

	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| aCollection aClass |
	self okToChange ifFalse: [^ self].
	(aCollection := self selectedMethods) ifEmpty: [^ self].
	(aClass := self selectedClass) ifNil: [^self].	

	self categorizeMethods: aCollection of: aClass from: aCollection first category.

	self updateCategoryAndMethodList.