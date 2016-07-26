actions
browseMessages
	self currentMethod ifNotNil: [ :method |
		 model browseMessagesFrom: method selector ]