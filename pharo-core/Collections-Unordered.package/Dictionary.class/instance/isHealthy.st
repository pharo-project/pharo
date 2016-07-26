testing
isHealthy
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise
	
	Dictionary allInstances select: [:dict |
		dict isHealthy not ]
	Dictionary allSubInstances select: [:dict |
		dict isHealthy not ]
	"
	array withIndexDo: [:elem :i |
		elem ifNotNil: [
			(self scanFor: elem key) == i ifFalse: [ ^ false ]
			]
	].
	^ true