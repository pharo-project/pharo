tests
testMetaclassSuperclass
	"self run: #testMetaclassSuperclass"

	self assert: Dictionary class superclass == Dictionary superclass class.
	self assert: OrderedCollection class superclass == OrderedCollection superclass class.