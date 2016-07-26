menus behavior
browseMessages
	self selectedMethod ifNil: [ ^ self inform: 'No method selected' ].
	self browseMessagesFrom: self selectedMethod selector