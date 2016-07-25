settings
useHTTPProxy
	"Should an HTTP proxy be used ? This is a global switch."

	^ UseHTTPProxy ifNil: [ UseHTTPProxy := false ]