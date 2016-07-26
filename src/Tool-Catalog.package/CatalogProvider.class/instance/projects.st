accessing
projects
	"Return the list of projects known in the central catalog.
	This data is retrieved over the internet.
	I will cache this data for 24 hours."
	
	self loadProjectsIfNeeded.
	^ projects ifNil: [ #() ]