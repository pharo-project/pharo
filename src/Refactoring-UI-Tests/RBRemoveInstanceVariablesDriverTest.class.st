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
