initialize
registerTextArea
	self hasTextArea
		ifTrue: [
			self textArea scrollPane: self.
			self updateScrollbars ]