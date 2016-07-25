testing
testDuplicateClassWithNewName
	| createdClass |
	createdClass := factory duplicateClass: TestCase withNewName: #MyTestClass.
	self assert: (factory createdClasses allSatisfy: [:class| self class environment includesKey: class name ]).
	factory cleanUp.
	self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]).
	self deny: (ChangeSet current changedClassNames includes: createdClass).