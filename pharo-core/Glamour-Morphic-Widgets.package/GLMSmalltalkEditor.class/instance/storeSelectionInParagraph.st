new selection
storeSelectionInParagraph
	super storeSelectionInParagraph.
	self morph ifNotNil: [
		self morph editView 
			announceSelection: self selection
			from: self startIndex 
			to: self stopIndex - 1 ]