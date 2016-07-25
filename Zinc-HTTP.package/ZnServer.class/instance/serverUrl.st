options
serverUrl
	"Return the explicitely set external server URL, if any. Defaults to nil."
	
	^ self optionAt: #serverUrl ifAbsent: [ nil ]