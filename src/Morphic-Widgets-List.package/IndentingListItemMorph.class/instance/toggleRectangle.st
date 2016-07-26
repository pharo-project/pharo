action
toggleRectangle

	| h |
	h := bounds height.
	^(bounds left + (13 * indentLevel)) @ bounds top extent: 9@h