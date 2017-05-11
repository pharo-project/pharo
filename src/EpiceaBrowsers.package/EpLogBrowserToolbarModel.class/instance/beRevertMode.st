accessing
beRevertMode

	filtersButtonModel
		icon: (self iconNamed: #smallUndoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserModel revertAllChangesAndCloseWindow ].
