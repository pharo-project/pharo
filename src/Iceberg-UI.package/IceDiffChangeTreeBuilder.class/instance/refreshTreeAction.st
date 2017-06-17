actions
refreshTreeAction
	^ GLMGenericAction new
		action: [ self entity refresh ];
		icon: #glamorousRefresh asIcon;
		shortcut: $r;
		title: 'Refresh tree'; showTitle