testing - navigation
testReferencedClasses
	{(ExceptionTester -> { MyTestNotification. Warning. String. MyResumableTestError. OrderedCollection. MyTestError}). 
	 (CollectionCombinator -> {Array}).
	 (ExecutionEnvironmentStub -> {OrderedCollection})
	}
		do: [ :assoc | 
			self assert: assoc key referencedClasses notEmpty.
			self assert: (assoc key referencedClasses asSet includesAll: assoc value asSet)]