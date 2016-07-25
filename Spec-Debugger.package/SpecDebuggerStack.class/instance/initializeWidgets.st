initialization
initializeWidgets

	listWidget := self newMultiColumnList.
	
	self list displayBlock: [ :aContext | self columnsFor: aContext ].
	self list menu: [ :aMenu |
		self contextMenu: aMenu]