actions
browseSendersOfMessage
	self currentMethod ifNotNil: [ :method |
		model browseSendersOfMessagesFrom: method selector ]