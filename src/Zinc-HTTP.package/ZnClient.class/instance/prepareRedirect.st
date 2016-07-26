private protocol
prepareRedirect
	"We implement the common behavior that POST/PUT redirects turn into GET request
	and that the body data is dropped, except for a 307 or 308 where this is explicitly forbidden.
	Note the cookie handling too."

	| targetUrl |
	targetUrl := self redirectUrl.
	self logRedirect: targetUrl.
	self receiveCookies.
	self prepareNextRequest.
	(#(307 308) includes: response code)
		ifFalse: [  
			(#(POST PUT) includes: request method)
				ifTrue: [ self method: #GET ].
			request hasEntity
				ifTrue: [ request resetEntity: nil ] ].
	self url: targetUrl.
	self sendCookies