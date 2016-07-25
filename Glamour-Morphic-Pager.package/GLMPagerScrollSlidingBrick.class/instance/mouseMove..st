event-mouse
mouseMove: evt

	isRightResizing
		ifTrue: [ self dragResizingRight: evt ]
		ifFalse: [
			isLeftResizing
				ifTrue: [ self dragResizingLeft: evt ]
				ifFalse: [ self dragSliding: evt ] ].