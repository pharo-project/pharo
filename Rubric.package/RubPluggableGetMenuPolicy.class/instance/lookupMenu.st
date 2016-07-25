accessing menu
lookupMenu
	(getMenuSelector isNil or: [ menuProvider isNil ])
		ifTrue: [ ^ super lookupMenu ].
	^ (WeakMessageSend receiver: menuProvider selector: getMenuSelector) cull: MenuMorph new cull: false