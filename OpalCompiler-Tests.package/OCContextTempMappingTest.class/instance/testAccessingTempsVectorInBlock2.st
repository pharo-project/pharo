tests
testAccessingTempsVectorInBlock2

	|a b r |
	a := [ r := 'failure'].
	[ b :='success'.
	r := thisContext ] on: Error do: a.

	self assert: r == (r tempNamed: 'r').
	self assert: (r tempNamed: 'a') == a.
	self assert: (r tempNamed: 'b') == b.