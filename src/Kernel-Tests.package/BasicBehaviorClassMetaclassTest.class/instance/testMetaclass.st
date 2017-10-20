tests
testMetaclass
	"self run: #testMetaclass"
	
	self assert: OrderedCollection class class == Metaclass.
	self assert: Dictionary class class == Metaclass.
	self assert: Object class class == Metaclass.




	
	
	



	
	

	