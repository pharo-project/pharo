accessing
beApplyMode

	filtersButtonModel
		icon: (self iconNamed: #smallRedoIcon);
		label: 'Confirm All Changes';
		action: [ logBrowserModel applyAllChangesAndCloseWindow ].
