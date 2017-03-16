initialization
initializeItemsModel

	itemsModel
		beMultipleSelection;
"		autoDeselect: true; -> not implemented yet?"
		displayBlock: [ :entryReference | (self itemForEntryReference: entryReference) asMorph ];
		whenSelectionChanged: [ self refreshEntryContentModelDeferrer schedule ];
		menu: [ self selectedItemsContextMenuMorph ].
		
	itemsModel announcer weak
		when: WidgetBuilt
		send: #initializeItemsModelPerformFilterWorkaround: to: self.
