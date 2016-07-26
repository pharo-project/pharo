accessing
edgeName: aSymbol
	"Set the value of edgeName. This is the edge of the target
	that will be manipulated by the grip."

	edgeName := aSymbol.
	self
		setLayoutSizingFor: aSymbol;
		layoutFrame: self gripLayoutFrame;
		layoutChanged