execute
execute
	|  message |
	message := UIManager default 
					multilineRequest: 'Enter description' initialAnswer: ''.
	(message isNil or: [ message isEmpty ])
		ifTrue: [ ^ self ].
	self executeWithMessage: message