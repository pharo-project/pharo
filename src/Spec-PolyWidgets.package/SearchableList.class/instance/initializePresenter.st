initialization
initializePresenter

	searchPresenter whenTextChanged: [:newText || text |
		text :=  searchPresenter getText asLowercase.
		text isEmpty
			ifTrue: [ listPresenter resetFilteringBlock ]
			ifFalse: [
				listPresenter filteringBlock: [:col | col select: [:e | e asLowercase beginsWith: searchPresenter getText asLowercase ]]]]