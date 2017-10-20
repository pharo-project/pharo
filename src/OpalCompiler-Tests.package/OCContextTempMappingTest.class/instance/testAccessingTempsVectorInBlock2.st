tests
testAccessingTempsVectorInBlock2

	|a b r |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].
	
	a := [ r := 'failure'].
	[ b :='success'.
	r := thisContext ] on: Error do: a.

	self assert: r == (r tempNamed: 'r').
	self assert: (r tempNamed: 'a') == a.
	self assert: (r tempNamed: 'b') == b.