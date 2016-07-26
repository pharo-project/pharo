initialize
unregisterTextArea
	self hasTextArea
		ifTrue: [ 
			self textArea scrollPane: nil.
			self textArea announcer unsubscribe: self.
			self textArea unplug ]