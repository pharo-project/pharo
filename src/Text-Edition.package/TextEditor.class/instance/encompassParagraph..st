private
encompassParagraph: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a cr (or eventually 1) and ends at a cr (or eventually at self size)"
	^ self string encompassParagraph: anInterval
