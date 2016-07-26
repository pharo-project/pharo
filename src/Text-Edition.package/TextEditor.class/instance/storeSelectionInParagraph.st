mvc compatibility
storeSelectionInParagraph
	self theme currentSettings haveSelectionTextColor 
		ifTrue: [
			self text removeAttribute: TextSelectionColor primarySelection.
			self text addAttribute: TextSelectionColor primarySelection from: self startIndex to: self stopIndex - 1].
	self paragraph selectionStart: self startBlock selectionStop: self stopBlock