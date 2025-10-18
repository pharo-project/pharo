Class {
	#name : 'RBRemoveMethodsRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveMethodsRefactoringTest >> testFailureMethodHasReferences [

	self shouldWarn: (ReRemoveMethodsRefactoring
								selectors: #(#checkClass:)
								from: RBBasicLintRuleTestData )
]

{ #category : 'tests' }
RBRemoveMethodsRefactoringTest >> testFailureNonExistantSelector [

	self shouldFail: (ReRemoveMethodsRefactoring
								selectors: #(#nonExistingMethod)
								from: RBLintRuleTestData )
]

{ #category : 'tests' }
RBRemoveMethodsRefactoringTest >> testRemovingMethodsFromDifferentClasses [

	| package class anotherClass methodWithoutRefs methodReferencing anotherMethodWithoutRefs |
	package := RBPackageEnvironment packageName:
		           RBBasicLintRuleTestData packageName.
	model := RBNamespace onEnvironment: package.
	class := model classFor: RBBasicLintRuleTestData.
	methodWithoutRefs := 'someDemoMethod' asSymbol.
	methodReferencing := 'onlyReferenceToSomeDemoMethod' asSymbol.
	anotherClass := model classFor: RBTransformationDummyRuleTest.
	anotherMethodWithoutRefs := 'justAUnreferencedMethodForTesting' asSymbol.

	self assert: (class definesMethod: methodWithoutRefs).
	self assert: (class definesMethod: methodReferencing).
	self assert: (anotherClass definesMethod: anotherMethodWithoutRefs).

	(ReRemoveMethodsRefactoring new
		model: model;
		classSelectorMapping: { 
			RBBasicLintRuleTestData -> methodWithoutRefs
			.
			RBBasicLintRuleTestData -> methodReferencing
			.
			RBTransformationDummyRuleTest -> anotherMethodWithoutRefs
		} 
	) generateChanges.

	self deny: (class definesMethod: methodWithoutRefs).
	self deny: (class definesMethod: methodReferencing ).
	self deny: (anotherClass definesMethod: anotherMethodWithoutRefs)
]

{ #category : 'tests' }
RBRemoveMethodsRefactoringTest >> testRemovingMethodsThatOnlyReferenceThemselves [

	| package class methodWithRef methodReferencing |
	package := RBPackageEnvironment packageName:
		           RBBasicLintRuleTestData packageName.
	model := RBNamespace onEnvironment: package.
	class := model classFor: RBBasicLintRuleTestData.
	methodWithRef := 'someDemoMethod' asSymbol.
	methodReferencing := 'onlyReferenceToSomeDemoMethod' asSymbol.

	self assert: (class definesMethod: methodWithRef).
	self assert: (class definesMethod: methodReferencing).

	(ReRemoveMethodsRefactoring
		 model: model
		 selectors: {
				 methodWithRef.
				 methodReferencing }
		 from: RBBasicLintRuleTestData) generateChanges.

	self deny: (class definesMethod: methodWithRef).
	self deny: (class definesMethod: methodReferencing)
]
