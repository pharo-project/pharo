as yet unclassified
render: aDropListPresentation
	listModel := GLMDropListModel new glamourPresentation: aDropListPresentation.	"When the morph changes, we want to update the glamour model"
	listModel announcer
		when: GLMDropDownListMorphSelectionChanged
		do: [ :ann | 
			aDropListPresentation selectedIndex: ann selectionIndex.
			aDropListPresentation selection: ann selectionValue ].
	listModel selectionIndex: aDropListPresentation selectedIndex.
	dropDownMorph := (MorphDropListMorph
		on: listModel
		list: #list
		selected: #selectionIndex
		changeSelected: #selectionIndex:
		useIndex: true)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	aDropListPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aDropListPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ dropDownMorph