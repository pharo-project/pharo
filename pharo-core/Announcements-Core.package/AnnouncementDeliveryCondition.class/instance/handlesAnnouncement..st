testing
handlesAnnouncement: anAnnouncement

	^(announcementClass handlesAnnouncement: anAnnouncement)
		and: [conditionBlock value: anAnnouncement ] 