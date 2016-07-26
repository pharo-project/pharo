tests
testReceiverGlobal
	| text context |
	text := 'testIt
	Dictionary n'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Dictionary class.
