*NECompletion
atCompletionPosition
	"Return true if the cursor is at a possible completion position"
	| cursorPosition  |
	cursorPosition := self startIndex.
	cursorPosition < 2 ifTrue: [ ^ false ].
	^ (self text at: cursorPosition - 1) isCompletionCharacter