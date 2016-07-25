accessing
cursorBounds

	temporaryCursor
		ifNil: [^ self position extent: NormalCursor extent]
		ifNotNil: [^ self position + temporaryCursorOffset
								extent: temporaryCursor extent]