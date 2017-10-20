tests - navigation
testMethodsReferencingClasses

	| collectionOfMethods collectionOfMethodsShouldBe |
	
	collectionOfMethods := ((ClassTest methodsReferencingClasses: {Smalltalk classNamed: #ExampleForTest12. Smalltalk classNamed: #ExampleForTest1}) sort: [ :a :b | a name <= b name]) asArray.
	
	collectionOfMethodsShouldBe := {
		ClassTest>>#referencingMethod1.
		ClassTest>>#referencingMethod2.
		ClassTest>>#testOrdersACollectionOfClassesBySuperclass}.
		
	self assert: collectionOfMethods asSet equals: collectionOfMethodsShouldBe asSet