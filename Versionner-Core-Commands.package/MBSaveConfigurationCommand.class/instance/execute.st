execute
execute
	| message |
	message := self uiManager multilineRequest: 'Enter commit message' initialAnswer: ''.
	message == nil
		ifTrue: [ ^ self ].
	self executeWithMessage: message