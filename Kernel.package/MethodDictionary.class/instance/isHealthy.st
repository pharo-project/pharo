testing
isHealthy
	"Test that selector hashes match their positions stored in dictionary,
	answer true if everything ok, false otherwise
	
	MethodDictionary allInstances select: [:dict |
		dict isHealthy not ]

	"
	1 to: self basicSize do: [:i | | selector |
		selector := self basicAt: i.
		selector ifNotNil: [
			(self scanFor: selector) == i ifFalse: [ ^ false ]]].
	^ true