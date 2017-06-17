testing - navigation
testMethodsReferencingClass
	self assert: (ClassTest methodsReferencingClass: (Smalltalk classNamed: #ExampleForTest111)) equals: {(ClassTest >> #testOrdersACollectionOfClassesBySuperclass)}.
	self
		assert: ((ClassTest methodsReferencingClass: (Smalltalk classNamed: #ExampleForTest1)) sort: [ :a :b | a name <= b name]) asArray
		equals:
			{ClassTest>>#referencingMethod1. 
			ClassTest>>#referencingMethod2. 
			ClassTest>>#testOrdersACollectionOfClassesBySuperclass}.
	self assert: (ClassTest methodsReferencingClass: (Smalltalk classNamed: #BehaviorTest)) isEmpty