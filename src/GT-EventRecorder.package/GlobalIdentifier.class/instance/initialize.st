initialization
initialize
	super initialize.
	"Dictionary contains all the values that we want to persist on disk."
	persistedInformation := Dictionary new.
	persistedInformation at: #computerUUID put: UUID new asString.
	persistedInformation at: #secretUUID put: UUID new asString.
	persistenceStrategy := GlobalIdentifierPersistence ston.
	persistenceStrategy previous: GlobalIdentifierPersistence fuel.