tests
testMetaclassName
	"self run: #testMetaclassName"

	self assert: Dictionary class  name = 'Dictionary class'.
	self assert: OrderedCollection class name = 'OrderedCollection class'.
	