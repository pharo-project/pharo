tests
testMetaclassNumberOfInstances
	"self run: #testMetaclassNumberOfInstances"

	self assert: Dictionary class allInstances size  = 1.
	self assert: OrderedCollection class allInstances size  = 1.