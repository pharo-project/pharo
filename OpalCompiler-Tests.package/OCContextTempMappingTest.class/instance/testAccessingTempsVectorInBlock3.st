tests
testAccessingTempsVectorInBlock3

	|a b r |
	a := [ r := 'failure'].
	[ b :='success'.
	self assert: (thisContext tempNamed: 'a') equals: a. 
	] on: Error do: a.
	self assert: (thisContext tempNamed: 'r') equals: nil.
	self assert: (thisContext tempNamed: 'b') equals: 'success'.