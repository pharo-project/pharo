field definition
compileAllFields
	"ExternalStructure compileAllFields"
	self withAllSubclassesDo:[:cls|
		cls compileFields.
	].