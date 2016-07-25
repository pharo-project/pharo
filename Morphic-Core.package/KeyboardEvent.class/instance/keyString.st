keyboard
keyString
	"Answer the string value for this keystroke. This is defined only for keystroke events."

	^ String streamContents: [ :s | self printKeyStringOn: s ]