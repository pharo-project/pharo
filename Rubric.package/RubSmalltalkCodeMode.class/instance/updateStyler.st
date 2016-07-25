shout
updateStyler

	self textArea ifNil: [ ^ self ].
	self textArea shoutStyler ifNotNil: [ :s | s classOrMetaClass: classOrMetaclass ]