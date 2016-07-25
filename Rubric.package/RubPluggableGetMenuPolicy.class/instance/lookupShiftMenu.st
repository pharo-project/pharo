accessing menu
lookupShiftMenu
	(getMenuSelector isNil or: [ menuProvider isNil ])
		ifTrue: [ ^ super lookupMenu ].
	^ (WeakMessageSend receiver: menuProvider selector: getMenuSelector) cull: MenuMorph new cull: true