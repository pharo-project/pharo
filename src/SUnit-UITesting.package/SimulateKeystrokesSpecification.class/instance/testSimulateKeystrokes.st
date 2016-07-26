tests
testSimulateKeystrokes

	| textMorph |
	textMorph := TextMorph new contents: ''; openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	self simulateKeyStrokes: 'hello'.
	self assert: textMorph contents = 'hello'.
	textMorph delete.