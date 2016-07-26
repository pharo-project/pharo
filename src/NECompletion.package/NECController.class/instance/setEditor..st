private
setEditor: anObject
	editor ifNotNil: [  
		"make sure we unsubscribe from old editor"
		editor morph ifNotNil: [:m | m announcer unsubscribe: self] ].
	editor := anObject.
	editor morph onAnnouncement: MorphLostFocus send: #closeMenu to: self.