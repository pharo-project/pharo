scrolling
okToStyle
	self shoutEnabled 
		ifFalse: [^ false].
	(model respondsTo: #shoutAboutToStyle:) 
		ifFalse: [^false].
	^model shoutAboutToStyle: self
