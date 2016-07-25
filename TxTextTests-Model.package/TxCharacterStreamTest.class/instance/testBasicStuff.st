tests
testBasicStuff

	| text stream |
	text := 'asdf' asTxModel.
	stream := text characterStream.

	self assert: stream next equals: $a.
	self assert: stream next equals: $s.
	self assert: stream next equals: $d.
	self assert: stream next equals: $f.
	self assert: stream next equals: nil.
	self assert: stream next equals: nil.

	self assert: stream isAtEnd.