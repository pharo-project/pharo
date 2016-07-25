acessing text
updateTextWith: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."

	self scrollPane updateTextWith: stringOrText.	
	"self setSelection: self getSelection."
	self resetState