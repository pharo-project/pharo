typing/selecting keys
displayIfFalse: aKeyboardEvent
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	ctrl-f."

	self addString: 'ifFalse:'.
	^false