copying
postCopy
	| newPort |
	super postCopy.
	self flag: 'self unregisterFromAllAnnouncements ?'.
	presentations := presentations copy.
	ports := ports collect: [:each | 
		newPort := each copy.
		newPort pane: self.
		self flag: 'what about value?'.
		newPort ]