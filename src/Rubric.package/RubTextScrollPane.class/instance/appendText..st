text area protocol
appendText: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	self textArea appendText: stringOrText.
	self scrollSelectionIntoView.
