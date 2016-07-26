event handling
whenTextChanged: anAnnouncement 
	| beg end delta |
	textArea ifNil: [ ^self ].
	beg := anAnnouncement start.
	end := anAnnouncement stop.
	delta := anAnnouncement text size - (end - beg + 1).
	self textChangedFrom: beg to: end delta: delta.
	textArea ifNil: [ ^self ].
	self computeVertices.
	self changed
