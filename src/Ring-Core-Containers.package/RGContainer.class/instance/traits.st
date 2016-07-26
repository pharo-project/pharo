convenient accesses
traits
	"Retrieves a collection (by default a dictionary) containing only defined traits"

	^self definedClasses select:[ :each| each isTrait ]