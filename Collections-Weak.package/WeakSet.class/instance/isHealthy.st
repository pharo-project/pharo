testing
isHealthy
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise
	
	WeakSet allSubInstances select: [:badSet |
		badSet isHealthy not ]
	"
	array withIndexDo: [ :element :index |
		(element isNotNil and: [ element ~~ flag ]) ifTrue: [
			(self scanFor: element) == index
				ifFalse: [ ^ false ]]].
	^ true