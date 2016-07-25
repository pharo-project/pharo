category
renameCategory: aCategory from: aClass
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| oldName newName |
	
	aClass ifNil: [^ nil].
	aCategory ifNil: [^ nil].
	
	oldName := aCategory.
	newName := self class requestProtocolNameFor: aClass initialAnswer: oldName.
	newName isEmptyOrNil
		ifTrue: [^ nil ]
		ifFalse: [newName := newName asSymbol].
	newName = oldName ifTrue: [^ nil ].
	aClass organization
		renameCategory: oldName
		toBe: newName.
	
	^ newName