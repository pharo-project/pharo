api-events
whenDateChanged: aBlock

	dateModel whenTextChanged: [:newText :oldText :announcement :anAnnouncer || newDate oldDate |
		newDate := Date readFrom: newText readStream.
		oldDate := Date readFrom: oldText readStream.
		aBlock cull: newDate cull: oldDate cull: announcement cull: anAnnouncer ]