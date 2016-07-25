protocol
openFloatingEditorToRenameAtIndex: anIndex
	| selectedRowMorph ed cat catMorph vScroll font cls |
	cls := self model selectedClass.
	cat := self getCategoryItem: anIndex.
	(cls organization protocolNamed: cat)
		ifNil: [ ^ self ]
		ifNotNil: [ :p | 
			p canBeRenamed
				ifFalse: [ ^ self ] ].
	vScroll := self vScrollValue.
	selectedRowMorph := categoriesList visibleRowMorphAtIndex: anIndex.
	catMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := catMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := categoriesList isVerticalScrollBarVisible
				ifTrue: [ categoriesList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(catMorph topLeft corner: right @ (selectedRowMorph bottom)) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(newName = cat asString or: [ newName isEmpty])
				ifFalse: [ self renameCategory: cat toBe: newName.
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: cat asString thenDo: [ :editor | editor selectAll ]