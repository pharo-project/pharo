scrollpane-scrolling
scrollToBottom

	self band hasSubbricks ifFalse: [ ^ self ].
	self scrollTo: self band lastSubbrick