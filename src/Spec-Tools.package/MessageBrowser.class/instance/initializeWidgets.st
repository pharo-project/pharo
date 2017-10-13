initialization
initializeWidgets

	listModel := self instantiate: FastTablePresenter.
	textModel := self newCode.
	toolbarModel := self instantiate: MethodToolbar.
	
	listModel numberOfColumns: 3 withWidths: #(250 250 50).
	listModel displayBlock: [ :item | self wrapItem: item ].
	textModel aboutToStyle: true.
	textModel whenBuiltDo: [ :ann | ann widget font: self codeFont ].
	refreshingBlockHolder := [ :item | true ] asValueHolder.
				
	self
		setListMenu;
		initializeDropList;
		setFocus.
