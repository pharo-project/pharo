accessing
addCookie: cookie
	self headers at: 'Set-Cookie' add: cookie fullString
