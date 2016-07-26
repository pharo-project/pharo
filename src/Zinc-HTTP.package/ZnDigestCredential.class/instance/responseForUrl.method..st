computation
responseForUrl: urlObject method: method
	^ ZnDigestAuthenticator md5Hash: self a1, ':', self nonce, ':', (self a2ForUrl: urlObject method: method)