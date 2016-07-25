options
clientId
	"Return the id for this HTTP client. Defaults to nil.
	Can be used identify different clients' log events."
	
	^ self optionAt: #clientId ifAbsent: [ nil ]