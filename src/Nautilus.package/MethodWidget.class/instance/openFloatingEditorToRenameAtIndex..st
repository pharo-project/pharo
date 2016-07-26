private
openFloatingEditorToRenameAtIndex: anIndex
	| selectedRowMorph ed itemRowMorph vScroll font cls method selector |
	method := self getMethods at: anIndex.
	selector := method selector.
	cls := method methodClass.
	vScroll := self vScrollValue.
	selectedRowMorph := methodsList visibleRowMorphAtIndex: anIndex.
	itemRowMorph := selectedRowMorph lastSubmorph lastSubmorph.
	font := itemRowMorph font.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith:
				[ :editor | 
			| right |
			editor font: font.
			editor scrollbarsShowNever.
			right := methodsList isVerticalScrollBarVisible
				ifTrue: [ methodsList verticalScrollBar left ]
				ifFalse: [ selectedRowMorph right ].
			editor
				bounds:
					(itemRowMorph topLeft 
						corner: right @ selectedRowMorph bottom ) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newName |
			newName := editedContents asString trimBoth.
			(cls selectors includes: newName asSymbol)
				ifTrue:
					[ newName asSymbol ~= selector
						ifTrue: [ self inform: newName , ' already exists in ' , cls name ] ]
				ifFalse:
					[ | oldArguments newMethodName |
					oldArguments := method ast arguments collect: #name.
					newMethodName := RBMethodName selector: newName asSymbol arguments: oldArguments.
					newMethodName isValid
						ifTrue:
							[ | refactor |
							refactor := self model refactor promptOnRefactoring: false.
							refactor renameMethodFor: method withNewMethodName: newMethodName ]
						ifFalse: [ self inform: 'Invalid method name' ].
					self vScrollValue: vScroll ] ].
	ed openEditorWithContents: selector asString thenDo: [ :editor | editor selectAll  ]