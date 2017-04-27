accessing
entriesCount
	"Answer the number of entries that this store contains"

	self checkIfMustRefresh.
	
	^ entryCount