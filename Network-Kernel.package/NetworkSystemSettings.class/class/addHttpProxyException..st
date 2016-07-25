settings
addHttpProxyException: domainName
	"Add a (partial, wildcard) domain name to the list of proxy exceptions"
	"HTTPSocket addProxyException: '*.online.disney.com'."

	self httpProxyExceptions add: domainName