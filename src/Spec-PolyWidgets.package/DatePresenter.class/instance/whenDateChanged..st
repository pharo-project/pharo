api-events
whenDateChanged: aBlock

	dateModel whenTextChanged: [ :newText :oldText :announcement :anAnnouncer |
		| oldDate |
		oldDate := date.
		date := newText
			ifNotEmpty: [ Date readFrom: newText readStream ]
			ifEmpty: [ nil ].
		aBlock cull: date cull: oldDate cull: announcement cull: anAnnouncer ]