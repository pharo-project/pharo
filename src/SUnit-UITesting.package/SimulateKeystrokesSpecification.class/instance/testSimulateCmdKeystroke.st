tests
testSimulateCmdKeystroke

	| ws |
	ws := Smalltalk tools workspace open.
	self simulateKeyStrokes: 'var := 3.'.
	self simulateKeyStroke: $s meta.
	self assert: ws hasUnacceptedEdits = false.
	ws delete.