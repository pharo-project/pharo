class
addCategoryIn: aClass before: aCategory
	"Present a choice of categories or prompt for a new category name 
	and add it before the current selection, or at the end if no current selection"

	| newName |
	newName := self class requestProtocolNameFor: aClass initialAnswer: ''.
	(newName isEmptyOrNil) ifTrue: [ ^ nil ].
	newName := newName asSymbol.

	aClass organization
		addCategory: newName
		before: aCategory.
	
	^ newName