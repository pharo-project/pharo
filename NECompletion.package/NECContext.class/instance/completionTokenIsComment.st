private-type-guessing
completionTokenIsComment
	| currentRange |
	currentRange := (ranges at: completionIndex).
	^  currentRange isComment