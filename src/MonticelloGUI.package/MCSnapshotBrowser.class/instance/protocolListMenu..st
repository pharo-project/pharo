menus
protocolListMenu: aMenu 
	protocolSelection
		ifNotNil: [aMenu
				add: ('Load protocol ''{1}''' format: {protocolSelection})
				selector: #loadProtocolSelection ].
	^ aMenu