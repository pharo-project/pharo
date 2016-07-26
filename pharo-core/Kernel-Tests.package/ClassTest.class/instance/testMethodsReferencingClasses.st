testing - navigation
testMethodsReferencingClasses

	| collectionOfMethods collectionOfMethodsShouldBe |
	
	collectionOfMethods := RBAddParameterTest methodsReferencingClasses: {RBLintRuleTest . RBBasicLintRuleTest}.
	
	collectionOfMethodsShouldBe := {RBAddParameterTest>>#testModelNonExistantName. 
	RBAddParameterTest>>#testBadInitializationCode. 
	RBAddParameterTest>>#testNonExistantName. 
	RBAddParameterTest>>#testModelBadInitializationCode}.
		
	self assert: collectionOfMethods asSet equals: collectionOfMethodsShouldBe asSet