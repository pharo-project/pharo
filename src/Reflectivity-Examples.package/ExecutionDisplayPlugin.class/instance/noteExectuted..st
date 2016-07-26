execution Visualizer
noteExectuted: anAnnouncement
	
	link uninstall.
	anAnnouncement method ifNotNil:[:method| method ast link: link].