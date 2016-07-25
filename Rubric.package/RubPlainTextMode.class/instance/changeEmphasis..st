text area emphasis
changeEmphasis: aSymbol
	"Change the alignment of the receiver. Alignment can be #normal, #bold,  #italic, #narrow, #underlined, or #struckOut"

	self changeAttribute: (TextEmphasis perform: aSymbol)
