parsing
withRelativeReference: reference
	"Return a new URL by parsing & resolving the relative reference using myself as base URL.
	This implements the process described in RFC 3986 Section 5"
	
	| resolved |
	[ (resolved := reference asZnUrl) isAbsolute 
		ifTrue: [ ^ resolved ] ] on: Error do: [ ].
	(reference beginsWith: '//')
		ifTrue: [ ^ self class fromString: (reference allButFirst: 2) defaultScheme: self scheme ].
	resolved := self copy.
	resolved fragment: nil.
	reference isEmpty ifTrue: [ ^ resolved ].
	reference first = $# ifFalse: [ resolved queryRemoveAll ].
	resolved processRelativeReference: reference.
	^ resolved