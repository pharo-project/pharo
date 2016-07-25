testing
isHealthy
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise
	
	Set allSubInstances select: [:badSet |
		badSet isHealthy not ]
	"
	array withIndexDo: [ :element :index |
		element ifNotNil: [
			(self scanFor: element enclosedSetElement) == index
				ifFalse: [ ^ false ]]].
	^ true