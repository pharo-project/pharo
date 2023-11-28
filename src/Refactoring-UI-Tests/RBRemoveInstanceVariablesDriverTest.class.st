Class {
	#name : 'RBRemoveInstanceVariablesDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRemoveInstanceVariablesDriverTest >> setUpDriver: driver [

	driver previewPresenterClass: StRefactoringPreviewPresenterMock.
	driver selectDialog: StSelectDialogMock new
]

{ #category : 'tests' }
RBRemoveInstanceVariablesDriverTest >> testBadPathVariableIsused [

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
RBRemoveInstanceVariablesDriverTest >> testGoodPathTwoUnusedVariables [
	
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
RBRemoveInstanceVariablesDriverTest >> testGoodPathVariableIsUnused [
	
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
