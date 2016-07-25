accessing
borderWidth: bw
	"Use narrowest image dimension."
	
	| newExtent |
	super borderWidth: bw.
	newExtent := 2 * bw + image extent min asPoint.
	bounds extent = newExtent ifFalse: [super extent: newExtent]