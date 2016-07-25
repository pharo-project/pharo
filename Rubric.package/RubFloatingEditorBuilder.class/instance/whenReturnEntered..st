announcement handling
whenReturnEntered: anAnnouncement
	acceptOnCR
		ifTrue: [ anAnnouncement accepted: true ].
	autoAccept
		ifTrue:
			[ | ann |
			ann := RubMorphEdited morph: editor.
			ann previousContents: initialContents.
			self announcer announce: ann yourself.
			self closeEditor ]