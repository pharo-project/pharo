testing
isHealthy
	"Test that object hashes match their positions stored in the receiver,
	answer true if everything ok, false otherwise
	
	WeakKeyDictionary allInstances select: [:dict |
		dict isHealthy not ]
	Dictionary allSubInstances select: [:dict |
		dict isHealthy not ]
	"
	array withIndexDo: [:elem :i |
		(elem notNil and: [elem key notNil ])
				ifTrue:  [(self scanFor: elem key) == i ifFalse: [ ^ false ]
			]
	].
	^ true