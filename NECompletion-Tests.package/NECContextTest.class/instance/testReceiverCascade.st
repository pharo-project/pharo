tests
testReceiverCascade
	| text context |
	text := 'testIt
	| b |
	b _ Stream new.
	b 
		nextPutAll: ''test''; 
		nextPut: $c;
		s'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Stream.
	text := 'testIt
	| b |
	b _ Stream new
		nextPutAll: ''test''; 
		nextPut: $c with: true;
		s'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Stream.
	text := 'testIt: aStream
	| test |
	aStream
		nextPutAll: ''test''; 
		nextPut: $c with: true;
		s'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Stream.
	text := 'testIt: aStream
	aStream
		nextPutAll: ''test''; 
		nextPut: $c with: true;
		s'.
	context := self createContextFor: text at: text size.
	self assert: context receiverClass == Stream