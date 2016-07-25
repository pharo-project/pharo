examples
exampleWithCustomCursor
	| text selection |
	text := 'abciuwhfi whifu wehifuhweifu wi ' asTxModel.
	selection := text startPosition selectTo: (text startPosition + 20).
	selection
		applyAttribute: (TxMouseCursorAttribute with: Cursor webLink);
		applyAttribute: (TxForeColorAttribute with: Color blue).
	^ self openInWindowWithText: text