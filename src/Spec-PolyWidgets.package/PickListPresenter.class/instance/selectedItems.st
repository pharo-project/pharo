api
selectedItems
	"Returns the selected items according to the list order"
	| wrappers items |
	
	wrappers := listPresenter listItems.
	items := wrappers collect: [:e || value |
		value := associationsWrapperToItem at: e ifAbsent: [ nil ].
		(value isNil or: [ e state not ])
			ifTrue: [ nil ]
			ifFalse: [ value ]] thenSelect: [ :e | e notNil ].
	^ items