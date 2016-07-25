typing support
openTypeIn
	"Set up startOfTyping to keep track of the leftmost backspace.
	 You can't undo typing until after closeTypeIn."

	self startOfTyping 
		ifNil: [self editingState previousInterval: (1 to: 0).
			self startOfTyping: self startIndex]