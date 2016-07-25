initialize-release
unplug
	self paragraphProvider
		ifNotNil: [ 
			self scrollPane ifNotNil: [:sp | sp announcer unsubscribe: self].
			self textArea ifNotNil: [:ts | ts announcer unsubscribe: self ] ].
	super unplug 