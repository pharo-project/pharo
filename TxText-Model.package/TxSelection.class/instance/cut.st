operations
cut
	
	"Delete the portion of text selected by receiver.
	Note, the receiver is in collapsed state afterwards!"
	| position |
	
	position := self text delete: self.
	self 
		start: position;
		end:  position.