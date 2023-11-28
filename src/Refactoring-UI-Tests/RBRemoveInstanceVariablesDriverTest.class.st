Class {
	#name : 'RBRemoveInstanceVariablesDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRemoveInstanceVariablesDriverTest >> testInstanceSmoke [
	
	| rbclasses refact |
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest  }.	
	refact := RBRemoveInstanceVariablesDriver new 
		scopes: { rbclasses }
		variables: { #instanceVariable }
		for: RBClassDataForRefactoringTest. 
	refact configureRefactoring
]

{ #category : 'tests' }
RBRemoveInstanceVariablesDriverTest >> testInstanceSmokeAssertChanges [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { RBLintRuleTestData . RBClassDataForRefactoringTest . MyClassARoot}.	
	RBRemoveInstanceVariablesDriver previewPresenterClass: StRefactoringPreviewPresenterMock.
	driver := RBRemoveInstanceVariablesDriver new 
		scopes: { rbclasses }
		variables: { #instVarName2 }
		for: MyClassARoot. 
	driver runRefactoring.
	RBRemoveInstanceVariablesDriver previewPresenterClass: StRefactoringPreviewPresenter.
	self assert: driver refactoring generateChanges isNotEmpty
	
]
