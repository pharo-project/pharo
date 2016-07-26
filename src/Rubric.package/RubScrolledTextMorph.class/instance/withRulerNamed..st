rulers handling
withRulerNamed: aKey
	"returns the ruler associated with the aKey. It can be in the receiver or in the extra hierarchy.
	aKey is a symbol i.e., #lineNumbers"
	(self hasRulerNamed: aKey)
		ifTrue: [ ^ self ].
	(self classOfRulerNamed: aKey)
		ifNotNil: [ :cls | self withRuler: cls new ]