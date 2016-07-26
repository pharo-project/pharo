options
dontFollowRedirects
	"Disable the default automatic following of HTTP redirects.
	Redirects will be returned as normal responses."

	^ self followRedirects: false