tests
testAPIFromMessageSendReceiver

	| constInitialResult constSecondResult |
	constInitialResult := self constFromReceiverExpression.
	constSecondResult := self constFromReceiverExpression.
	
	self assert: constSecondResult == constInitialResult