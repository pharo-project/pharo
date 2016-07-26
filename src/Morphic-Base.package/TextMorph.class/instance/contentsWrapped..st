accessing
contentsWrapped: stringOrText
	"Accept new text contents.  Lay it out, wrapping within my current width.
	Then fit my height to the result."
	wrapFlag := true.
	self newContents: stringOrText