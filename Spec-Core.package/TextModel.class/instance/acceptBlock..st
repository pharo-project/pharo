api
acceptBlock: aBlockWithOneArgument
	"<api: #block getter: #acceptBlock registration: #whenAcceptBlockChanged:>"
	"Set the block to perform when the text is accepted.
	The block must have one argument, which will be the accepted text.
	The block can return false if the candidate text is to be rejected"

	actionToPerformHolder value: aBlockWithOneArgument