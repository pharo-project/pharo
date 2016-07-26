protocol
openFloatingEditorToRenameAtIndex: anIndex
	| selectedRowMorph ed clsMorph vScroll font cls |
	cls := self model selectedClass theNonMetaClass.
	vScroll := self vScrollValue.
	selectedRowMorph := classesList visibleRowMorphAtIndex: anIndex.
	clsMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := clsMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := classesList isVerticalScrollBarVisible
				ifTrue: [ classesList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(clsMorph topLeft corner: right @ selectedRowMorph bottom) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(newName = cls name asString or: [ newName isEmpty])
				ifFalse: [self renameClass: cls toBe: newName.
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: cls name asString thenDo: [ :editor | editor selectAll  ]