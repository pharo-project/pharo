initialization
initializeWidgets

	self title: 'Changes Browser'.

	changes := self newList.
	changes displayBlock: [ :elem | self changeDescription: elem ].
	changes beMultipleSelection.

	selectAllButton := self newButton.
	selectAllButton
		label: 'select all'.

	deselectAllButton := self newButton.
	deselectAllButton
		label: 'deselect all'.
		
	fileInSelectedButton := self newButton.
	fileInSelectedButton
		label: 'file in selected'.

	codePane := self newCode.