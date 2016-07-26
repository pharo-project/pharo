announcement handling
whenTextAccepted: anAnnouncement
	self
		editorDo:
			[ :ed | 
			| ann |
			autoAccept
				ifFalse: [ self closeEditor ].
			ann := RubMorphEdited morph: ed.
			ann previousContents: initialContents.
			self announcer announce: ann ]