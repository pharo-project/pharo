initialization
initialize
	super initialize.
	packages := OrderedCollection new. 
	self browsedEnvironment packageOrganizer announcer weak when: RPackageUnregistered send: #packageRemoved: to: self