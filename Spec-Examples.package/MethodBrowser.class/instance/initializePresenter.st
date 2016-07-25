initialization
initializePresenter

	listModel whenSelectedItemChanged: [:selection |
		selection
			ifNil: [
				textModel text: ''.
				textModel behavior: nil.
				toolbarModel method: nil ]
			ifNotNil: [:m | 
				textModel text: m sourceCode.
				textModel behavior: m methodClass.
				toolbarModel method: m ]].
	
	self acceptBlock: [:t | self listModel selectedItem inspect ].
	self wrapWith: [:item | item methodClass name,'>>#', item selector ].