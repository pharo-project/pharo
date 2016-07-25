keyboard
keyboardPeek
	"Answer the next character in the keyboard buffer without removing it, or nil if it is empty."

	^ self characterForEvent: self peekKeyboardEvent