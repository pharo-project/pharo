writing
nextPutImage: aForm
	"Write out the given form. We're keeping it simple here, no interlacing, no filters."
	^self nextPutImage: aForm interlace: 0 filter: 0. "no filtering"