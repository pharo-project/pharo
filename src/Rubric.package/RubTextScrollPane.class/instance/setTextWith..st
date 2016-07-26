text area protocol
setTextWith: stringOrText
	"Set the initial text contents with line breaks only as in the text.
	Fit my width and height to the result."
	self textArea width: self scroller width.
	self textArea setTextWith: stringOrText.
	self updateScrollbars 
