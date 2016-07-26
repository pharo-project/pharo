instance creation
newForEncoding: aString
	| encoding |
	aString ifNil: [ ^ Latin1TextConverter new ].
	encoding := aString asLowercase.
	^ self allSubclasses
		detect: [ :each | each encodingNames includes: encoding ]
		ifFound: [ :class | class new ]
		ifNone: [ nil ]