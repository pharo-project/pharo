text area alignment
changeAlignment: aSymbol
	"Change the alignment of the receiver. Alignment can be #leftFlush #centered #rightFlush #justified "
	self changeAttribute: (TextAlignment perform: aSymbol)