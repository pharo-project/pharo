Class {
	#name : 'RBRemoveInstanceVariablesDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI',
	#package : 'Refactoring-UI'
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
