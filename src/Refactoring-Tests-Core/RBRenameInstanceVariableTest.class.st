Class {
	#name : #RBRenameInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBRenameInstanceVariableTest >> testAlreadyExistingName [
	self shouldFail: (RBRenameInstanceVariableRefactoring 
			rename: 'classBlock'
			to: 'name'
			in: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBRenameInstanceVariableTest >> testNonExistantName [
	self shouldFail: (RBRenameInstanceVariableRefactoring 
			rename: 'foo'
			to: 'newFoo'
			in: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBRenameInstanceVariableTest >> testRenameInstVar [
	| refactoring class |
	refactoring := RBRenameInstanceVariableRefactoring rename: 'classBlock' to: 'asdf' in: RBBasicLintRuleTestData.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self
		assert:
			(class parseTreeFor: #checkClass:)
				equals:
					(self
						parseMethod:
							'checkClass: aSmalllintContext 
								^asdf value: aSmalllintContext value: result').
	self
		assert:
			(class parseTreeFor: #initialize)
				equals:
					(self
						parseMethod:
							'initialize
	super initialize.
	self anInstVar: 1.
	asdf := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment')
]

{ #category : #tests }
RBRenameInstanceVariableTest >> testRenameInstVarNotAccessors [
	| refactoring class |
	refactoring := RBRenameInstanceVariableRefactoring rename: 'result' to: 'whatever' in: RBBasicLintRuleTestData renameAccessors: false.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'whatever').
	self deny: (class directlyDefinesInstanceVariable: 'result').
	
	self deny: (class definesMethod: #whatever).
	self deny: (class definesMethod: #whatever:).
	self assert: (class definesMethod: #result).

]
