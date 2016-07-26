text area protocol
updateTextWith: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	self textArea width: self scroller width.
	self textArea updateTextWith: stringOrText.
	self updateScrollbars 
