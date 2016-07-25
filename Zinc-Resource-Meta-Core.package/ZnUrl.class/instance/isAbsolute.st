testing
isAbsolute
	"We consider URLs with scheme://host as absolute (port is not relevant here).
	See also #inContextOf: and #withRelativeReference:"
	
	^ self hasScheme and: [ self hasHost ]