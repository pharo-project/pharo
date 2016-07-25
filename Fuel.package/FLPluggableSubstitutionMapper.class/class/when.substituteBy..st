instance creation
when: aCondition substituteBy: aFactory

	^self basicNew
		initializeWith: aCondition 
		substitutionFactory: aFactory;
		yourself