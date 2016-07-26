as yet unclassified
contents: stringOrText wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	wrapFlag := true.
	super extent: width truncated@self height.
	self newContents: stringOrText