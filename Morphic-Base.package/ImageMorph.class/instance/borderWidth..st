accessing
borderWidth: bw

	| newExtent |
	newExtent := 2 * bw + image extent.
	bounds extent = newExtent ifFalse: [super extent: newExtent].
	super borderWidth: bw