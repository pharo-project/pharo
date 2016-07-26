computation
a2ForUrl: urlObject method: method
	"This doesn't support all qop options yet."
	^ ZnDigestAuthenticator md5Hash: method, ':', (self uriPathFrom: urlObject)