initialization
registerChangeActions
	
	changesListPresenter
		whenSelectedItemChanged: [:change | 
			classesListPresenter resetSelection.
			self updateTextContents.
			change 
				ifNil: [
					classesListPresenter items: {}.
					methodsListPresenter items: {}.
					]
				ifNotNil: [
					classesListPresenter items: (change changedClasses sort: [:a :b | a name < b name ]).
					 ]].
	