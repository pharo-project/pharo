class initialization
initialize
	"ByteTextConverter initialize"

	self allSubclassesDo: [:subclass |
		subclass initializeTables]