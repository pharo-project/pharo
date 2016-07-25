typing/selecting keys
backWord
	"If the selection is not a caret, delete it and leave it in the backspace buffer.
	 Else, delete the word before the caret."
	^ self backWord: nil
