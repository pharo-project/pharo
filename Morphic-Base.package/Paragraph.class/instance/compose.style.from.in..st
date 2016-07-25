composition
compose: t style: ts from: startingIndex in: textContainer
	text := t.
	textStyle := ts.
	firstCharacterIndex := startingIndex.
	offsetToEnd := text size - firstCharacterIndex.
	container := textContainer.
	self composeAll