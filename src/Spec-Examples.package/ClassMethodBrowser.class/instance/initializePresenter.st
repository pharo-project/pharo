initialization
initializePresenter
	
	listModel whenSelectedItemChanged: [:selection |
		selection 
			ifNotNil: [:class | 
				methodModel methods: (class methods sort: [:a :b | a selector < b selector]).
				methodModel listModel resetSelection ]].