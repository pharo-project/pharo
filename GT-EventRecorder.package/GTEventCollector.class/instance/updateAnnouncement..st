updating
updateAnnouncement: aGTEventAnnouncement 
	updateAnnouncementBlock ifNil: [ ^ aGTEventAnnouncement ].
	updateAnnouncementBlock cull: aGTEventAnnouncement.
	^ aGTEventAnnouncement