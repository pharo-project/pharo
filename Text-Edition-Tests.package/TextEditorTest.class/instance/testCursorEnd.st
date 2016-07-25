accessing
testCursorEnd
	| editor |
	"
	self debug: #testCursorEnd.
	"
	editor := TextMorph new editor.
	editor cursorEnd: self keyboardEvent