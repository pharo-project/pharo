accessing
checker
	"Return a GlobalIdentifierPersistenceChecker object."
	^ checker ifNil: [ checker := GlobalIdentifierPersistenceChecker new ]