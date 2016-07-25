accessing
trackerUrl
	^ ('https://pharo.fogbugz.com/default.asp?', self number asString) asUrl
	