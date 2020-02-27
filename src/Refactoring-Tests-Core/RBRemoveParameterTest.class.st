Class {
	#name : #RBRemoveParameterTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBRemoveParameterTest >> testNonExistantName [
	self
		shouldFail: (RBRemoveParameterRefactoring 
				removeParameter: 'asdf'
				in: RBBasicLintRuleTestData
				selector: #checkClass:);
		shouldFail: (RBRemoveParameterRefactoring 
				removeParameter: 'aSmalllintContext'
				in: RBBasicLintRuleTestData
				selector: #checkClass1:)
]

{ #category : #'failure tests' }
RBRemoveParameterTest >> testPrimitiveMethods [
	| refactoring |
	(model classNamed: #Object) 
		compile: 'foo123: a <primitive: 100> ^#() primitiveFailed'
		classified: #(#accessing).
	refactoring := RBRemoveParameterRefactoring 
				model: model
				removeParameter: 'a'
				in: Object
				selector: #foo123:.
	self shouldFail: refactoring
]

{ #category : #tests }
RBRemoveParameterTest >> testRemoveParameter [
	| refactoring class |
	refactoring := RBRemoveParameterRefactoring 
		removeParameter: 'anArg'
		in: RBRefactoryTestDataApp
		selector: ('rename' , 'ThisMethod:') asSymbol.
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #renameThisMethod) equals: (self parseMethod: 'renameThisMethod
								^self').
	self assert: (class parseTreeFor: #callMethod) equals: (self parseMethod: 'callMethod
								^(self renameThisMethod)').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)
]
