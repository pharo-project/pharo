accessing
borderStyle: newStyle
	"Use narrowest image dimension."
	
	| newExtent |
	self borderStyle = newStyle ifTrue: [^self].
	super borderStyle: newStyle.
	newExtent := 2 * newStyle width + image extent min asPoint.
	bounds extent = newExtent ifFalse: [self extent: newExtent]