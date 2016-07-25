converting
inContextOf: absoluteUrl
	"Return a copy of the receiver where scheme, host and port
	are taken from absoluteUrl unless they are already in the receiver.
	Path merging is not supported. See also #withRelativeReference: "
	
	| copy |
	copy := self copy.
	copy hasScheme ifFalse: [ copy scheme: absoluteUrl scheme ].
	copy hasHost ifFalse: [ copy host: absoluteUrl host ].
	(copy hasPort not and: [ copy scheme = absoluteUrl scheme ]) 
		ifTrue: [ copy port: absoluteUrl port ].      
	^ copy