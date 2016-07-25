initialization
initializePresenter

	searchModel whenTextChanged: [:newText || text |
		text :=  searchModel getText asLowercase.
		text isEmpty
			ifTrue: [ listModel resetFilteringBlock ]
			ifFalse: [
				listModel filteringBlock: [:col | col select: [:e | e asLowercase beginsWith: searchModel getText asLowercase ]]]]