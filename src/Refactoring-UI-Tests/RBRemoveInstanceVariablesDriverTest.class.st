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
	rbclasses := RBClassEnvironment classes: { RBLintRuleTestData  }.	
	driver := RBRemoveInstanceVariablesDriver new 
		scopes: { rbclasses }
		variables: { #foo1 }
		for: RBClassDataForRefactoringTest. 
	driver configureRefactoring.
	driver runRefactoring.
	self assert: driver changes isNotEmpty
	
]
