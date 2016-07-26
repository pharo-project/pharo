accessing-selection
storeSelectionInText
	self theme currentSettings haveSelectionTextColor 
		ifTrue: [
			self text removeAttribute: RubTextSelectionColor primarySelection.
			self text addAttribute: RubTextSelectionColor primarySelection from: self startIndex to: self stopIndex - 1]