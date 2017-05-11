initialization
initializeItemsModel

	itemsModel
		beMultipleSelection;
"		autoDeselect: true; -> not implemented yet?"
		displayBlock: [ :entryReference | self morphForItemAt: entryReference ];
		whenSelectionChanged: [ self refreshEntryContentModelDeferrer schedule ];
		menu: [ self menuMorphForSelectedItems ].
		
	itemsModel announcer weak
		when: WidgetBuilt
		send: #initializeItemsModelPerformFilterWorkaround: to: self.
