Class {
	#name : 'RBRemoveMethodRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRemoveMethodRefactoringTest >> testFailureRemoveReferenced [

	self shouldWarn: (RBRemoveMethodRefactoring
			 model: model
			 selector: #checkClass:
			 from: #RBBasicLintRuleTestData)
]

{ #category : 'failure tests' }
RBRemoveMethodRefactoringTest >> testFailureRemoveSameMethodButSendsSuper [

	self shouldFail: (RBRemoveMethodRefactoring
			 model: model
			 selector: #new
			 from: #'RBBasicLintRuleTestData class')
]

{ #category : 'tests' }
RBRemoveMethodRefactoringTest >> testJustSendsSuper [

	| refactoring selector |
	selector := #justSuperSendInSubclass.
	refactoring := RBRemoveMethodRefactoring
		               model: model
		               selector: selector
		               from: #RBSubclassOfClassToRename.

	self assert: ((model classNamed: #RBSubclassOfClassToRename)
			 directlyDefinesMethod: selector).

	self assert: (refactoring justSendsSuper: selector)
]

{ #category : 'tests' }
RBRemoveMethodRefactoringTest >> testJustSendsSuperFailing [

	| refactoring selector |
	selector := #justSuperSendInSubclassBroken.
	refactoring := RBRemoveMethodRefactoring
		               model: model
		               selector: selector
		               from: #RBSubclassOfClassToRename.

	self assert: ((model classNamed: #RBSubclassOfClassToRename)
			 directlyDefinesMethod: selector).
	self deny: (refactoring justSendsSuper: selector)
]

{ #category : 'tests' }
RBRemoveMethodRefactoringTest >> testModelRecursiveMethodThatIsNotReferencedFromOtherMethods [

	| class otherClass selector |
	class := model classNamed: #Object.
	class
		compile: 'someMethodThatIsNotReferenced ^2'
		classified: #( #accessing ).
	selector := 'someMethodThatIsNotReferenced' asSymbol.

	self assert: (class definesMethod: selector).
	otherClass := model metaclassNamed: self class superclass name.
	otherClass
		compile:
		'someMethodThatIsNotReferenced ^3 someMethodThatIsNotReferenced'
		classified: #( #accessing ).
	model removeClassNamed: self class name.

	(RBRemoveMethodRefactoring
		model: model
		selector: selector
		from: #Object) generateChanges.

	self deny: (class definesMethod: selector)
]

{ #category : 'tests' }
RBRemoveMethodRefactoringTest >> testSendersIncludesSuperCalls [

	| refactoring |
	refactoring := RBRemoveMethodRefactoring
		               model: model
		               selector: #justSuperSendInSubclass
		               from: #RBSubclassOfClassToRename.
	self denyEmpty: (refactoring sendersOf: #justSuperSendInSubclass)
]

{ #category : 'tests' }
RBRemoveMethodRefactoringTest >> testSendersOfUnsentMessage [

	| refactoring rbclasses rbNamespace2 |
	rbclasses := RBClassEnvironment classes: {
			             RBClassToRename.
			             RBSubclassOfClassToRename }.
	rbNamespace2 := RBNamespace onEnvironment: rbclasses.

	refactoring := RBRemoveMethodRefactoring
		               model: model
		               selector: #methodThatIsNotUsedForSure
		               from: #RBSubclassOfClassToRename.
	refactoring model: rbNamespace2.
	self assertEmpty: (refactoring sendersOf: #methodThatIsNotUsedForSure)
	"this is because this test method is taken into account."
]
