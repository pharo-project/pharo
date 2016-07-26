public composition
replaceFrom: start to: stop with: aText
	"Edit the text, and then recompose the lines."
	self text replaceFrom: start to: stop with: aText.
	self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop - start + 1).
