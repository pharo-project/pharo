copying
postCopy
	"Don't share the reference to the sole instance."

	super postCopy.
	thisClass := nil.