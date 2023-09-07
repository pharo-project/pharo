Class {
	#name : #RBRemoveMethodsRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRemoveMethodsRefactoringTest >> testFailureMethodHasReferences [

	self shouldWarn: (RBRemoveMethodsRefactoring
								selectors: #(#checkClass:)
								from: RBBasicLintRuleTestData )
]

{ #category : #tests }
RBRemoveMethodsRefactoringTest >> testFailureNonExistantSelector [

	self shouldFail: (RBRemoveMethodsRefactoring
								selectors: #(#nonExistingMethod)
								from: RBLintRuleTestData )
]

{ #category : #tests }
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

	(RBRemoveMethodsRefactoring
		 model: model
		 selectors: {
				 methodWithRef.
				 methodReferencing }
		 from: RBBasicLintRuleTestData) generateChanges.

	self deny: (class definesMethod: methodWithRef).
	self deny: (class definesMethod: methodReferencing)
]
