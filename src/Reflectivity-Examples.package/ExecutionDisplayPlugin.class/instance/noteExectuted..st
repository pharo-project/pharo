execution Visualizer
noteExectuted: anAnnouncement
	(self isMethodAlreadyLinked: anAnnouncement method)
		ifTrue: [ ^ self ].
	link uninstall.
	anAnnouncement method ifNotNil: [ :method | method ast link: link ]