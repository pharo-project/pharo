accessing
borderStyle: newStyle
	"Set the extent to include border width."
	
	| newExtent |
	self borderStyle = newStyle ifTrue: [^self].
	newExtent := 2 * newStyle width + image extent.
	bounds extent = newExtent ifFalse: [super extent: newExtent].
	super borderStyle: newStyle