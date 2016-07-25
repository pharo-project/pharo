ui requests
syntaxErrorNotificationDefaultAction: anException
	"Handle a syntax error.
	Note, if there is no #syntaxErrorDebugger registered, it will be silently ignored"
	^Smalltalk tools using: #syntaxErrorDebugger do: [:tool | 
			tool open: (tool syntaxError: anException)]