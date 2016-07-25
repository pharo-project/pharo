editing helpers
changeEmphasis: aSymbol
	"Change the alignment of the receiver. Alignment can be #normal, #bold,  #italic, #narrow, #underlined, or #struckOut"
	
	"| t |
t := 'khkjhjkhjkhjkh
kjhj
kjhkjhkjh
kjh
kjhkjh kjh jh jh jh jh kjh kjh ' asTextMorph.
t openInWorld.
t selectAll; changeEmphasis: #bold.
t inspect"
	
	self editor applyAttribute: (TextEmphasis perform: aSymbol).
	"this code should be moved to TextEditor but changeEmphasis: is defined there for something else
	so it should be renamed first there."
	self updateFromParagraph