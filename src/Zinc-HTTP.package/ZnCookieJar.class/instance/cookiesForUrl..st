accessing
cookiesForUrl: urlObject
	^ self cookies select: [ :cookie | 
		(cookie isExpired not) & (cookie isInDomain: urlObject) & (cookie isInPath: urlObject) ]