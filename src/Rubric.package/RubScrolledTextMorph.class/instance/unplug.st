private
unplug
	self textArea ifNotNil: [ self textArea announcer unsubscribe: self ].
	self scrollPane
		ifNotNil: [ :scrollpane | 
			scrollpane announcer unsubscribe: self.
			scrollpane unplug ].
	super unplug