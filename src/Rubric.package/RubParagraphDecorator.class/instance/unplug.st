initialize-release
unplug
	next
		ifNotNil: [ 
			next unplug.
			next := nil ].
	super unplug