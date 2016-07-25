submorphs-add/remove
delete
	textArea ifNotNil: [ :ta | ta removeSegment: self ].
	textArea := nil.
	super delete