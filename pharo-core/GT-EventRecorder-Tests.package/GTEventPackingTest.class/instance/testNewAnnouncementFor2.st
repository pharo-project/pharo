tests-packing
testNewAnnouncementFor2
	collector updateAnnouncementBlock: [ :anAnnouncement | 
		anAnnouncement at: #testNewAnnouncementFor2 put: 42 ].
	announcement := packing newAnnouncementFor: collector.
	self assert: announcement serializedBy equals: packing class name.
	self assert: announcement eventRecorderVersion equals: packing version.
	self assert: (announcement at: #testNewAnnouncementFor2) equals: 42.