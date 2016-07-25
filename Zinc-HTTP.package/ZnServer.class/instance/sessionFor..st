accessing
sessionFor: request
	"Bind an existing session to request or create a new session"
	
	^ self sessionManager sessionFor: request