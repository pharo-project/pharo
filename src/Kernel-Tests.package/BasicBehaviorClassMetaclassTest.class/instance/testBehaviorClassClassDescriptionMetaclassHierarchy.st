tests
testBehaviorClassClassDescriptionMetaclassHierarchy
	"self run: #testBehaviorClassClassDescriptionMetaclassHierarchy"
	
	self assert: Class superclass  == ClassDescription.
	self assert: Metaclass superclass == ClassDescription.

	self assert: ClassDescription superclass  == Behavior.
	self assert: Behavior superclass  = Object.

	self assert: Class class class ==  Metaclass.
	self assert: Metaclass class class  == Metaclass.
	self assert: ClassDescription class class == Metaclass.
	self assert: Behavior class class == Metaclass.




	
	
	



	
	

	