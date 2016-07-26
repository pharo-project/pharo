testing
isCaretBehindChar
	"Return true if the cursor position is after an alphanumeric character, otherwise false."

	| cursorPosition |
	^(cursorPosition := self startIndex) >= 2 and: [
		(self text at: cursorPosition - 1) isAlphaNumeric ]