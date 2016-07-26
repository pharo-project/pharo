repositories-options
disableRepositoryErrors
	"Silently swallow all repository errors."

	errorBlock := [ :error | error resume: #() ]