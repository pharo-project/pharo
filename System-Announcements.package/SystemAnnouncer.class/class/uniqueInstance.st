instance creation
uniqueInstance
	"Answer a system annoncer."
	
	^ announcer ifNil: [ announcer := self new ]