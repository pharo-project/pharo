instance creation
fromString: aString for: urlObject
	| cookie |
	cookie := self fromString: aString.
	"Ensures a domain value if none provided by the cookie"
	cookie defaultDomain: urlObject host.
	^ cookie