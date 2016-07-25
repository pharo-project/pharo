private
receiveCookies
	response cookies do: [ :each |
		each defaultDomain: request url host.
		self session cookieJar add: each ]