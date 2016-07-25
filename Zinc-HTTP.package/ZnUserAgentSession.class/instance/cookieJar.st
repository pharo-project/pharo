accessing
cookieJar
	^ cookiejar ifNil: [ cookiejar := ZnCookieJar new ]