private
loadProjectsIfNeeded
	self shouldLoadProjects ifFalse: [ ^ self ].
	projects := self loadProjects.
	session := Smalltalk session.
	lastModified := DateAndTime now