editing helpers
changeAlignment: aSymbol
	"Change the alignment of the receiver. Alignment can be #leftFlush #centered #rightFlush #justified "
	
	"| t |
t := 'khkjhjkhjkhjkh
kjhj
kjhkjhkjh
kjh
kjhkjh kjh jh jh jh jh kjh kjh ' asTextMorph.
t openInWorld.
t selectAll; changeFormatting: #rightFlush.
t inspect"
	
	self editor applyAttribute: (TextAlignment perform: aSymbol).
	self updateFromParagraph