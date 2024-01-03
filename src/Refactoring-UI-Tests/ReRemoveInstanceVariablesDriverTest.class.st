Class {
	#name : 'ReRemoveInstanceVariablesDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReRemoveInstanceVariablesDriverTest >> testBadPathVariableIsused [

	| rbclasses driver |
	"the variable is used so there is only one changes."
	rbclasses := RBClassEnvironment classes: {
			             RBLintRuleTestData.
			             RBClassDataForRefactoringTest.
			             MyClassARoot }.
	driver := RBRemoveInstanceVariablesDriver new
		          scopes: { rbclasses }
		          variables: { #instVarName1 }
		          for: MyClassARoot.
	self setUpDriver: driver.
	driver runRefactoring.
	self assert: driver breakingChoices size equals: 3
]

{ #category : 'tests' }
ReRemoveInstanceVariablesDriverTest >> testGoodPathTwoUnusedVariables [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { RBLintRuleTestData . RBClassDataForRefactoringTest . MyClassARoot}.	
	driver := RBRemoveInstanceVariablesDriver new 
		scopes: { rbclasses }
		variables: { #unusedVar1 . #unusedVar2 }
		for: MyClassARoot. 
	self setUpDriver: driver.
	driver runRefactoring.
	self assert: driver refactoring changes changes size equals: 2
	
]

{ #category : 'tests' }
ReRemoveInstanceVariablesDriverTest >> testGoodPathVariableIsUnused [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { RBLintRuleTestData . RBClassDataForRefactoringTest . MyClassARoot}.	
	driver := RBRemoveInstanceVariablesDriver new 
		scopes: { rbclasses }
		variables: { #unusedVar2 }
		for: MyClassARoot. 
	self setUpDriver: driver.
	driver runRefactoring.
	self assert: driver refactoring changes changes size equals: 1
	
]
