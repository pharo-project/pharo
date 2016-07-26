tests
testReceiverClassVar
	| text context |
	text := 'testIt
	ClassVar '.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString