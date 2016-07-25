accessing
authTokenForUrl: urlObject method: method
	^ 'Basic ', (ZnUtils encodeBase64: (username, ':', password)) 