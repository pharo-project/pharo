acessing text
setTextWith: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	self scrollPane setTextWith: stringOrText.
	self setSelection: self getSelectionFromModel.
	self resetState