initialization
reinitializeAllSubclasses
	"self reinitializeAllSubclasses"
	self allSubclassesDo: [ :each | each initialize ]