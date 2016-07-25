tests
testReceiverTempVar3
"if tempvar assignment is from a keyword message send (case 3), the guessed type should be 'nil' as in case for simple messages. Don't move over to other var assignments (smallint for var b)"
	| text context |
	"case 1 guess from constant"
	text := 'testIt
	| a b |
	a _ ''test''.
	b _ 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	"case 2 can not guess from return value of a message send"
	text := 'testIt
	| a b |
	a _ self class.
	b _ 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	"case 3 can not guess from return value of a keyword message send"
	text := 'testIt
	| a b |
	a _ self init:''test''.
	b _ 10.
	a as'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
