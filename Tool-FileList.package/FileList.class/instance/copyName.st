menu messages
copyName

	listIndex = 0 ifTrue: [^ self].
	Clipboard clipboardText: self fullName asText.
