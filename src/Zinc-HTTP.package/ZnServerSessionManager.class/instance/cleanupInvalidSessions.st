accessing
cleanupInvalidSessions
	"Remove all expired sessions"
	
	sessions keysAndValuesRemove: [ :key :value | value isValid not ]