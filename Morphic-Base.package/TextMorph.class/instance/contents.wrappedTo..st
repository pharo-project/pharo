accessing
contents: stringOrText wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	self newContents: ''.
	wrapFlag := true.
	super extent: width truncated@self height.
	self newContents: stringOrText