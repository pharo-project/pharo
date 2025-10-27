Extension { #name : 'ErrorHandlerSessionHandler' }

{ #category : '*UIManager' }
ErrorHandlerSessionHandler >> shutdown: isImageQuitting [

	"Prepare the shutdown and the next startup"

	UIManager default: StartupUIManager new.
	ErrorHandler defaultErrorHandler: UIManager default.
]

{ #category : '*UIManager' }
ErrorHandlerSessionHandler >> startup: isImageStarting [
	"Install the right UIManager"

	UIManager default: UIManager forCurrentSystemConfiguration.
	ErrorHandler defaultErrorHandler: UIManager default.
]
