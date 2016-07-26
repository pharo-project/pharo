accessing
setSession: session in: response
	"Bind session in response"

	| cookie |
	session ifNil: [ ^ self ].
	(cookie := ZnCookie name: self cookieName value: session id)
		path: '/'.
	response addCookie: cookie