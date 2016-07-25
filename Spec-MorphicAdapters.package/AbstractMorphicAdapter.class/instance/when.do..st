protocol
when: anAnnouncement do: aBlock

	self widgetDo: [ :w | w announcer
		when: anAnnouncement do: aBlock ]