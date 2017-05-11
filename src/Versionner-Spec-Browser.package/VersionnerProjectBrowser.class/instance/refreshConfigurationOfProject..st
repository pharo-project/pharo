refreshing
refreshConfigurationOfProject: anAnnouncement
	configuration = anAnnouncement configurationInfo
		ifTrue: [ self refreshConfiguration ]