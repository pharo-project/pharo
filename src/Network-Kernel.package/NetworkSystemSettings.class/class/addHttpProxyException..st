settings
addHttpProxyException: domainName
	"Add a (partial, wildcard) domain name to the list of proxy exceptions"
	"HTTPSocket addProxyException: '*.pharo.org'."

	self httpProxyExceptions add: domainName