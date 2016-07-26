accessing request
url: urlObject
	"Modify the URL of the current request object to urlObject.
	This is done using ZnUrl>>#inContextOf: so that either scheme, 
	host, port or path are set and merged.
	Default the URL using ZnUrl>>#asZnUrlWithDefaults to fall back
	to http or the default http/https ports."
	
	| url |
	url := urlObject asZnUrl.
	request url
		ifNotNil: [ url := url inContextOf: request url ].  
	url := url asZnUrlWithDefaults.  
	self validateConnectionForHost: url host port: url port.   
	request url: url.
	url hasUsername & url hasPassword
		ifTrue: [ self setBasicAuthenticationUsername: url username password: url password ]