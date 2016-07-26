some tests
testAllSetsAreHealthy
	| badSets |
	"Only run for Set"
	self collectionClass = Set
		ifFalse: [ ^ self ].
	badSets := Set allSubInstances select: [:badSet | 
		badSet isHealthy not ].
	self assert: badSets isEmpty.