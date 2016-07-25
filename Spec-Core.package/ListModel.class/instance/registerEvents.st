initialization
registerEvents

	listHolder whenChangedDo: [ self refreshListItems ].
	filteringBlockHolder whenChangedDo: [ self refreshListItems ].
	selectionHolder index whenChangedDo: [ self changed: #getIndex ].
	multiSelection whenChangedDo: [ :b | self changed: { #setMultipleSelection: . b } ].
	wrapBlockHolder whenChangedDo:[ self changed: #listElementAt: ].
	backgroundColorBlock whenChangedDo: [ self refreshListItems ].
	autoDeselect whenChangedDo: [ :boolean | self changed: #autoDeselect: with: { boolean }].
	sortingBlockHolder
		whenChangedDo: [ 
			listHolder value: (listHolder value sorted: sortingBlockHolder value).
			self changed: #listElementAt: ].
