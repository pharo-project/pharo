tests
testMetaclassPointOfCircularity
	"self run: #testMetaclassPointOfCircularity"

	self assert: Metaclass class instanceCount = 1.
	self assert: Metaclass class someInstance == Metaclass.


	
	
	



	
	

	