accessing
basicLog
	"Answer a new EpLog with a OmFileStore (not a OmSessionStore as #log)."

	^ EpLog newWithStore: self sessionStore store