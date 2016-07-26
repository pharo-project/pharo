copying
postCopy
	super postCopy.
	pane := nil.
	self flag: 'self unregisterFromAllAnnouncements.'.
	updateActions := updateActions
		collect: [ :each | 
			(each copy)
				presentation: self;
				yourself ].
	rawSelectionTransmissions := rawSelectionTransmissions collect: [:each | each copy].
	rawSelectionTransmissions do: [:transmission | transmission originReferences do: [:portRef | portRef port presentation: self]].
	rawSelectionTransmissions do: [:transmission | transmission destination presentation: self]