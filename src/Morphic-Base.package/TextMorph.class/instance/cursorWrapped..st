accessing
cursorWrapped: aNumber
	"Set the cursor as indicated"

	self setProperty: #textCursorLocation toValue: (((aNumber rounded - 1) \\  text string size) + 1)

	