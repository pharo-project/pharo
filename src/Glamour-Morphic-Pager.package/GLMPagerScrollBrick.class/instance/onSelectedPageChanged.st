event-handling
onSelectedPageChanged
	
	self panes do: [ :each | each borderColor: Color transparent ].
	
	self theme pagerWidgetThemer selectedPaneStyleFor: (self panes at: pagerModel selectedPageIndex)