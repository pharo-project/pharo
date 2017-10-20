testing
testDuplicateClassWithNewName
	| createdClass |
	
	"Skip this tests if no compiler is available. Do not use #skip because it breaks an announcements tests"
	
	Smalltalk compilerClass ifNotNil: [ 	
		createdClass := factory duplicateClass: TestCase withNewName: #MyTestClass.
		self assert: (factory createdClasses allSatisfy: [:class| self class environment includesKey: class name ]).
		factory cleanUp.
		self assert: (factory createdClasses allSatisfy: [:class| class isObsolete]).
		self deny: (ChangeSet current changedClassNames includes: createdClass).]
	
