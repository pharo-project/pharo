tests
testReceiverTempVar
	| text context |
	text := 'testIt
	| aha |
	aha _ ''test''.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha |
	ah _ ''test''.
	ah p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	text := 'testIt
	| aha |
	aha _ constantString.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha |
	aha _ constant asDate.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass isNil.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ aha.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == ByteString.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ aha _ 15.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass = SmallInteger.
	text := 'testIt
	| aha bili |
	aha _ constantString.
	bili _ 15.
	bili _ aha.
	bili p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == SmallInteger.
	text := 'testIt
	[ :each | |a| a _ 16. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == SmallInteger.
	text := 'testIt
	[ :each | |a| a _ Dictionary new. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == Dictionary.
	text := 'testIt
	[ :each | |a| a _ Dictionary. a print'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass == Dictionary class.
	text := 'testIt
	| aha |
	aha _ constantLargeInteger.
	aha p'.
	context := self 
		createContextFor: text
		at: text size.
	self assert: context receiverClass = LargePositiveInteger.