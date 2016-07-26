repositories-options
enableRepositoryErrors
	"Throw an exception when repositories are not available."

	errorBlock := [ :error | error pass ]