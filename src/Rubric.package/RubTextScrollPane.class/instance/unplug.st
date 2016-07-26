private
unplug
	self hasTextArea
		ifTrue: [ self textArea unplug ].
	super unplug