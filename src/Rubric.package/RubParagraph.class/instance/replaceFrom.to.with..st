editing
replaceFrom: start to: stop with: aText
	"Edit the text, and then recompose the lines."

	self
		disableDrawingWhile: [ 
			self composer emphasisHere: textArea emphasisHere.
			self composer replaceFrom: start to: stop with: aText.
			self text: self composer text.
			textArea paragraphWasComposedFrom: start to: stop.
			textArea paragraphReplacedTextFrom: start to: stop with: aText ]