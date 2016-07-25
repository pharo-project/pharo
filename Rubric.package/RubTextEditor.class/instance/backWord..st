typing/selecting keys
backWord: aKeyboardEvent
	"If the selection is not a caret, delete it and leave it in the backspace buffer.
	 Else, delete the word before the caret."

	| startIndex |
	self hasCursor ifTrue: [ "a caret, delete at least one character"
		startIndex := 1 max: self markIndex - 1.
		[startIndex > 1 and:
			[(self string at: startIndex - 1) tokenish]]
				whileTrue: [
					startIndex := startIndex - 1]]
	ifFalse: [ "a non-caret, just delete it"
		startIndex := self markIndex].
	self backTo: startIndex.
	^false