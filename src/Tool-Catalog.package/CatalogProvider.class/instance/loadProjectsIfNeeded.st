private
loadProjectsIfNeeded
	self shouldLoadProjects
		ifFalse: [ ^ self ].
	projects := self loadProjects.
	lastModified := DateAndTime now