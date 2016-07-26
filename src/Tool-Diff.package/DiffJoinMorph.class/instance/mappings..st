accessing
mappings: anObject
	"Set the value of mappings"

	mappings := anObject.
	self updateMappings.
	self changed