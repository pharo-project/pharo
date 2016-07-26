options
followRedirects: boolean
	"Set whether or not I will automatically following of HTTP redirects.
	This is still subject to #maxNumberOfRedirects. Defaults to true.
	When set to false, redirects will be returned as normal responses."

	self optionAt: #followRedirects put: boolean 