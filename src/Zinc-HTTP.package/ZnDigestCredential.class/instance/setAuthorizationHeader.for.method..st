operations
setAuthorizationHeader: headers for: urlObject method: method
	headers at: 'Authorization' put: (self authTokenForUrl: urlObject method: method)