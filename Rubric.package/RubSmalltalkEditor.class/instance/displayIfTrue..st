typing/selecting keys
displayIfTrue: aKeyboardEvent
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	ctrl-t."

	self addString: 'ifTrue:'.
	^false