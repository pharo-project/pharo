running-copying
testCopy
	"Ensure that the actionMap is zapped when
	you make a copy of anEventManager"

	eventSource when: #blah send: #yourself to: eventListener.
	self assert: eventSource actionMap keys isEmpty not.
	self assert: eventSource copy actionMap keys isEmpty