accessing
sessionManager
	"Return my session manager"

	^ sessionManager ifNil: [ sessionManager := ZnServerSessionManager new ]