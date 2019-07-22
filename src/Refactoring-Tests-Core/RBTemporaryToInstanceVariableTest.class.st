Class {
	#name : #RBTemporaryToInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBTemporaryToInstanceVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #'failure tests' }
RBTemporaryToInstanceVariableTest >> testNonExistantName [
	self
		shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: RBBasicLintRuleTestData
				selector: #checkClass:
				variable: 'asdf');
		shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: RBBasicLintRuleTestData
				selector: #checkClass1:
				variable: 'aSmalllintContext')
]

{ #category : #'failure tests' }
RBTemporaryToInstanceVariableTest >> testRedefinedTemporary [
	| class |
	class := model classNamed: #Foo.
	class 
		compile: 'someMethod | instVarName1 | instVarName1 := 4. ^instVarName1'
		classified: #(#accessing).
	self shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: class
				selector: #someMethod
				variable: 'instVarName1')
]

{ #category : #tests }
RBTemporaryToInstanceVariableTest >> testTemporaryToInstanceVariable [
	| refactoring class |
	refactoring := RBTemporaryToInstanceVariableRefactoring 
		class: RBLintRuleTestData
		selector: #displayName
		variable: 'nameStream'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class parseTreeFor: #displayName) equals: (self parseMethod: 'displayName
								nameStream := WriteStream on: (String new: 64).
								nameStream
									nextPutAll: self name;
									nextPutAll: '' (''.
								self problemCount printOn: nameStream.
								nameStream nextPut: $).
								^nameStream contents').
	self assert: (class directlyDefinesInstanceVariable: 'nameStream')
]
