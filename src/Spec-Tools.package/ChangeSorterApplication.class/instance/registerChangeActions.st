initialization
registerChangeActions
	
	changesListModel
		whenSelectedItemChanged: [:change | 
			classesListModel resetSelection.
			self updateTextContents.
			change 
				ifNil: [
					classesListModel items: {}.
					methodsListModel items: {}.
					]
				ifNotNil: [
					classesListModel items: (change changedClasses sort: [:a :b | a name < b name ]).
					 ]].
	