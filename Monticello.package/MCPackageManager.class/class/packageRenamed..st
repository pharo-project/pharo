system changes
packageRenamed: anAnnouncement
	self allManagers 
		detect: [ :each | each packageName = anAnnouncement newName ]
		ifFound: [ :newPackage | newPackage modified: true ].
	(self allManagers 
		detect: [ :each | each packageName = anAnnouncement oldName ])
		unload.