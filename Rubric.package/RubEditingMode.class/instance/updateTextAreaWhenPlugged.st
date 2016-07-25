initialize-release
updateTextAreaWhenPlugged
	self textArea
		restoreSelectionIndexesAfter: [ 
			| t |
			t := self textArea text copy.
			self textArea updateTextWith: ''.
			self textArea updateTextWith: t ]