tests
testReceiverGlobalVarNew
	| text context |
	text := 'testIt
	Dictionary new a'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Dictionary.
